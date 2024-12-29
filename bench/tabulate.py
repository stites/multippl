#!/usr/bin/env python3
import argparse
import time
import os, sys
import numpy as np
import scipy.stats
from rich.console import Console
from rich.table import Table
from datetime import datetime
import json
import os.path


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

def get_summary_json(logroot, subex):
    for dir in experiment_folders(logroot, subex):
        logtime = dir.strftime(subex + "/%Y-%m-%d/%H:%M")
        rundir = f"{logroot}/{logtime}"
        raw = f"{rundir}.data.json"
        if not os.path.isfile(raw):
            print(f"warning! {raw} not found! Please run:")
            bench_dir = os.path.abspath(os.path.join(os.path.abspath(__file__), ".."))
            print(f"  cd {bench_dir}/{subex} && python ./avg.py --logdir {logroot}")
        else:
            with open(raw) as f:
                sjson = json.loads(f.read())
            yield sjson

def summary_union(ss):
    final = dict()
    all_keys = set()
    for summary in ss:
        ks = {k for k, v in summary.items() if len(v) == 0}
        if len(all_keys & ks) > 0:
            raise Exception("found overlapping experiments, please review logdir structure")
        else:
            all_keys = all_keys & ks
        final = final | summary
    return final


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

    results = dict()
    for ex in EXPERIMENTS:
        for subex in os.listdir(f"{args.logdir}/{ex}/"):
            subex = f"{ex}/{subex}"
            results[subex] = summary_union(get_summary_json(args.logdir, subex))

    console = Console(record=True)
    (h, d, w) = full_table(results)

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
        if file not in stats or len(stats[file]) == 0:
            return ["---"] * ncols
        elif stats[file]["counts"] == 0:
            timeout_counts[ex][file] = final_num_runs
            return ["t/o"] * ncols
        else:
            if stats[file]["counts"] < final_num_runs:
                timeout_counts[ex][file] = final_num_runs - stats[file]["counts"]

            sc_mean = stats[file]["seconds"]["mean"]
            sc_err = stats[file]["seconds"]["stderr"]
            if with_l1:
                l1_mean = stats[file]["l1"]["mean"]
                l1_err = stats[file]["l1"]["stderr"]
                return (f"{l1_mean:.3f} ±{l1_err:.3f}", f"{sc_mean:.3f} ±{sc_err:.3f}")
            else:
                return (f"{sc_mean:.3f} ±{sc_err:.3f}",)

    for n in [15, 31, 63]:
        try:
            exp = f"arrival/tree-{n}"
            ss = results[exp]["stats"]
            timeout_counts[exp] = dict()
            hybrid.add_row(
                exp,
                *stat_or_timeout(exp, ss, "main.psi"),
                *stat_or_timeout(exp, ss, "main.py"),
                *stat_or_timeout(exp, ss, "cont.yo"),
                *stat_or_timeout(exp, ss, "main.yo"),
                end_section=(n == 63),
            )
        except:
            pass

    for n in ["alarm", "insurance"]:
        try:
            exp = f"bayesnets/{n}"
            ss = results[exp]["stats"]
            timeout_counts[exp] = dict()
            hybrid.add_row(
                n,
                *stat_or_timeout(exp, ss, "main.psi"),
                *stat_or_timeout(exp, ss, "main.py"),
                *stat_or_timeout(exp, ss, "cont.yo"),
                *stat_or_timeout(exp, ss, "main.yo"),
                end_section=(n == "insurance"),
               )
        except:
            pass
    for n in [4, 10, 20]:
        try:
            exp = f"gossip/g{n}"
            ss = results[exp]["stats"]
            timeout_counts[exp] = dict()
            hybrid.add_row(
                f"gossip/{n}",
                *stat_or_timeout(exp, ss, "main.psi"),
                *stat_or_timeout(exp, ss, "main.py"),
                *stat_or_timeout(exp, ss, "cont.yo"),
                *stat_or_timeout(exp, ss, "main.yo"),
                end_section=(n == 20),
               )
        except:
            pass

    discrete = Table(
        "# Nodes",
        "PSI (s)",
        "MultiPPL (Disc, s)",
        "Pyro (l1)",
        "Pyro (s)",
        "MultiPPL (Cont, l1)",
        "MultiPPL (Cont, s)",
        "MultiPPL (l1)",
        "MultiPPL (s)",
        title="Discrete Benchmark",
        title_justify="left",
    )
    for n in [3, 6, 9]:
        try:
            exp = f"grids/{n}x{n}"
            ss = results[exp]
            timeout_counts[exp] = dict()
            discrete.add_row(
                f"{n*n}",
                *stat_or_timeout(exp, ss, "main.psi", with_l1=False),
                *stat_or_timeout(exp, ss, "exact.yo", with_l1=False),
                *stat_or_timeout(exp, ss, "main.py"),
                *stat_or_timeout(exp, ss, "cont.yo"),
                *stat_or_timeout(exp, ss, "diag.yo"),
                end_section=(n == 9),
               )
        except:
            pass

    warnings = ""
    timeouts = [x > 0 for v in timeout_counts.values() for x in v.values()]
    if any(timeouts):
        warnings += "Warning! the following programs timed out:\n"
        for k, tos in timeout_counts.items():
            warnings += f"  {k}\n"
            for f, ts in tos.items():
                warnings += f"    {f}: {ts}\n"

    return hybrid, discrete, warnings


def endsin(f):
    return f.split(".")[-1] in ["py", "psi", "yo"]


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
