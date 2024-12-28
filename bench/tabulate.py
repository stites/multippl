#!/usr/bin/env python3
import argparse
import time
import os, sys
import numpy as np
import scipy.stats
from rich.console import Console
from rich.table import Table
from datetime import datetime


RESERVED = [
    "bench.py",
    "main.dice",
    "avg.py",
    "utils.py",
    "stdin2l1.py",
    "yo2l1.py",
    "truth.py",
    "truth.sh",
    "main.dice-partial",
]
EXPERIMENTS = {"grids"}
#EXPERIMENTS = {"bayesnets", "arrival", "grids", "gossip"}
needs_l1 = lambda main: main[-3:] == ".yo" or main[-3:] == ".py"


def experiment_folders(logroot, subex):
    allruns = []
    for day in filter(lambda f: os.path.isdir(f"{logroot}/{subex}/{f}"), os.listdir(f"{logroot}/{subex}")):
        for hm in filter(
            lambda t: os.path.isdir(f"{logroot}/{subex}/{day}/{t}"),
            os.listdir(f"{logroot}/{subex}/{day}"),
        ):
            allruns.append(datetime.strptime(f"{day} {hm}", "%Y-%m-%d %H:%M"))
    return allruns


def merge_experiments(logroot, subex):
    for dir in experiment_folders(logroot, subex):
        logtime = dir.strftime(subex + "/%Y-%m-%d/%H:%M")
        logdir = f"{logroot}/{logtime}"
        for log in filter(lambda l: l[-4:] == ".log", os.listdir(logdir)):
            yield logdir, log


def get_mainfiles(expdir):
    return [
        f
        for f in os.listdir(expdir)
        if os.path.isfile(f"{expdir}/{f}") and not (f in RESERVED) and endsin(f)
    ]


def get_truth(expdir):
    sys.path.append(expdir)
    from main import truth
    sys.path.pop()
    return truth


def get_mainfile(mainfiles, log, expdir):
    main = [m for m in mainfiles if log[: len(m)] == m.replace(".", "-")]
    if len(main) == 0:
        raise Exception(
            f"Did not find a main file associated with {log}. Re-inspect how you invoked benchmarks, or delete this folder."
        )
    else:
        assert len(main) == 1, f"did not find a main file associated with {log}"
    return main[0]


def get_output_and_seconds(logdir, log):
    with open(logdir + "/" + log, "r") as f:
        out = f.readlines()
        out = "".join(out).rstrip().split("\n")
    print(out)
    if len(out) == 1 and out[0] == "":
        return None
        #raise Exception(f"incomplete run: {logdir}/{log} is empty!")
    lastline_ms = out[-1]
    if lastline_ms == "":
        raise Exception(f"last is empty! full output is:\n{out}")
    if lastline_ms[-2:] != "ms":
        raise Exception(f"expected {log} to have last line ending in 'ms'")

    if lastline_ms[: len("timeout")] == "timeout":
        return None
    else:
        # PSI outputs can vary, ms cannot
        try:
            val = [float(f) for f in out[-2].rstrip().split()]
        except:
            val = []
        print((lastline_ms[:-2]))
        secs = float(lastline_ms[:-2]) / 1000.0
        return val, secs


def main(args):
    subdirs = list(
        filter(lambda f: os.path.isdir(args.logdir + "/" + f), os.listdir(args.logdir))
    )
    runs_all_experiments = all([ex in subdirs for ex in EXPERIMENTS])

    if not runs_all_experiments:
        print(
            "ambiguous log folder structure. Expecting <logdir>/[<experiment>/]<%Y-%m-%d>/"
        )
        sys.exit(1)

    ss = []
    results = dict()
    for ex in EXPERIMENTS:
        for subex in os.listdir(f"{args.logdir}/{ex}/"):
            subex = f"{ex}/{subex}"
            expdir = os.path.dirname(os.path.abspath(__file__)) + f"/{subex}"

            mainfiles = get_mainfiles(expdir)
            truth = get_truth(expdir)
            stats = {k: {"count": 0, "sec": [], "l1": []} for k in mainfiles}

            for logdir, log in merge_experiments(args.logdir, subex):
                try:
                    main = get_mainfile(mainfiles, log, expdir)
                    output = get_output_and_seconds(logdir, log)
                except:
                    continue
                if output is None:
                    # we tabulate these
                    #print(f"got timeout for {log}")
                    pass
                else:
                    #if "main.py" != main:
                    #    continue
                    #print(output)
                    out, sec = output
                    ss.append(sec)
                    stats[main]["sec"].append(sec)
                    stats[main]["count"] += 1
                    if needs_l1(main):
                        l1 = sum(compute_l1(out, truth))
                        stats[main]["l1"].append(l1)
                    else:
                        stats[main]["l1"].append(0.0)
            results[subex] = dict(stats=stats, mainfiles=mainfiles)
    print(np.array(ss).mean())
    console = Console(record=True)
    (h, d, w) = full_table(results)
    print(w)
    with open(f"{args.logdir}/timeouts", "w") as f:
        f.write(w + "\n")

    console.print(h)
    console.save_text(f"{args.logdir}/hybrid.rich")

    console.print(d)
    console.save_text(f"{args.logdir}/discrete.rich")
    print(
        f"output saved to {args.logdir}/timeouts, {args.logdir}/hybrid.rich, {args.logdir}/discrete.rich"
    )


def full_table(results):
    final_num_runs = 100
    hybrid = Table(
        "Model",
        "PSI (l1)",
        "PSI (s)",
        "Pyro (l1)",
        "Pyro (s)",
        "MultiPPL (Cont, l1)",
        "MultiPPL (Cont, s)",
        "MultiPPL (l1)",
        "MultiPPL (s)",
        title="Hybrid Benchmark",
        title_justify="left",
    )

    timeout_counts = dict()

    def stat_or_timeout(ex, stats, file, with_l1=True):
        ncols = 2 if with_l1 else 1
        if file not in stats:
            return ["---"] * ncols
        elif stats[file]["count"] == 0:
            timeout_counts[ex][file] = final_num_runs
            return ["t/o"] * ncols
        else:
            if stats[file]["count"] < final_num_runs:
                timeout_counts[ex][file] = final_num_runs - stats[file]["count"]

            sc_mean, sc_err = mean_and_stderr(stats[file]["sec"])
            if with_l1:
                l1_mean, l1_err = mean_and_stderr(stats[file]["l1"])
                return (f"{l1_mean:.3f} ±{l1_err:.3f}", f"{sc_mean:.3f} ±{sc_err:.3f}")
            else:
                return (f"{sc_mean:.3f} ±{sc_err:.3f}",)

    #for n in [15, 31, 63]:
    #    exp = f"arrival/tree-{n}"
    #    ss = results[exp]["stats"]
    #    timeout_counts[exp] = dict()
    #    hybrid.add_row(
    #        exp,
    #        *stat_or_timeout(exp, ss, "main.psi"),
    #        *stat_or_timeout(exp, ss, "main.py"),
    #        *stat_or_timeout(exp, ss, "cont.yo"),
    #        *stat_or_timeout(exp, ss, "main.yo"),
    #        end_section=(n == 63),
    #    )

    #for n in ["alarm", "insurance"]:
    #    exp = f"bayesnets/{n}"
    #    ss = results[exp]["stats"]
    #    timeout_counts[exp] = dict()
    #    hybrid.add_row(
    #        n,
    #        *stat_or_timeout(exp, ss, "main.psi"),
    #        *stat_or_timeout(exp, ss, "main.py"),
    #        *stat_or_timeout(exp, ss, "cont.yo"),
    #        *stat_or_timeout(exp, ss, "main.yo"),
    #        end_section=(n == "insurance"),
    #    )

    #for n in [4, 10, 20]:
    #    exp = f"gossip/g{n}"
    #    ss = results[exp]["stats"]
    #    timeout_counts[exp] = dict()
    #    hybrid.add_row(
    #        f"gossip/{n}",
    #        *stat_or_timeout(exp, ss, "main.psi"),
    #        *stat_or_timeout(exp, ss, "main.py"),
    #        *stat_or_timeout(exp, ss, "cont.yo"),
    #        *stat_or_timeout(exp, ss, "main.yo"),
    #        end_section=(n == 20),
    #    )

    discrete = Table(
        "# Nodes",
        #"PSI (s)",
        #"MultiPPL (Disc, s)",
        "Pyro (l1)",
        "Pyro (s)",
        "MultiPPL (Cont, l1)",
        "MultiPPL (Cont, s)",
        "MultiPPL (l1)",
        "MultiPPL (s)",
        title="Discrete Benchmark",
        title_justify="left",
    )
    for n in [3]: #, 6, 9]:
        exp = f"grids/{n}x{n}"
        ss = results[exp]["stats"]
        timeout_counts[exp] = dict()
        discrete.add_row(
            f"{n*n}",
            #*stat_or_timeout(exp, ss, "main.psi", with_l1=False),
            #*stat_or_timeout(exp, ss, "exact.yo", with_l1=False),
            *stat_or_timeout(exp, ss, "main.py"),
            *stat_or_timeout(exp, ss, "cont.yo"),
            *stat_or_timeout(exp, ss, "diag.yo"),
            end_section=(n == 9),
        )
    warnings = ""
    timeouts = [x > 0 for v in timeout_counts.values() for x in v.values()]
    if any(timeouts):
        warnings += "Warning! the following programs timed out:\n"
        for k, tos in timeout_counts.items():
            warnings += f"  {k}\n"
            for f, ts in tos.items():
                warnings += f"    {f}: {ts}\n"

    return hybrid, discrete, warnings


def compute_l1(ms, truth):
    return list(map(lambda x: abs(x[0] - x[1]), zip(ms, truth)))


def endsin(f):
    return f.split(".")[-1] in ["py", "psi", "yo"]


def mean_and_stderr(data):
    if len(data) == 1:
        return data[0], 0
    else:
        a = 1.0 * np.array(data)
        return np.mean(a), (0 if len(data) < 1 else scipy.stats.sem(a))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="tabulate results from logs/")
    parser.add_argument(
        "--logdir",
        default="logs/",
        type=str,
    )
    experiment_name = os.path.dirname(__file__).split("/")[-1]
    main(parser.parse_args())
else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
