#!/usr/bin/env python3
import argparse
import time
import os, sys
import numpy as np
import scipy.stats
from rich.console import Console
from rich.table import Table
from datetime import datetime

sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from main import truth

DEVELOP=False
def compute_l1(ms, truth):
    return list(map(lambda x: abs(x[0] - x[1]), zip(ms, truth)))

reserved = ["bench.py", "avg.py", "utils.py", "yo2l1.py", "truth.py", "truth.sh", "main.dice-partial"]
def endsin(f):
    return f.split(".")[-1] in ["py", "psi", "yo"]
mainfiles = [f for f in os.listdir('.') if os.path.isfile(f) and not (f in reserved) and endsin(f)]
needs_l1 = lambda main: main[-3:] == ".yo" or main[-3:] == ".py"
pad = max([len(m) for m in mainfiles])

def mean_and_stderr(data):
    a = 1.0 * np.array(data)
    return np.mean(a), scipy.stats.sem(a)

def str_stats(ss):
    s = []
    for metric in ['l1', 'ms', ' #']:
        for m in mainfiles:
            if ss[m][' #'] == 0:
                continue
            if metric == " #":
                out = ss[m][metric]
                s.append("{} {}: {}".format(m.ljust(pad, " "), metric, str(out)))
            else:
                mean, stderr = mean_and_stderr(ss[m][metric])
                s.append("{} {}: {} ±{}".format(m.ljust(pad, " "), metric, str(mean).ljust(pad+10, " "), str(stderr)))
            s.append("\n")
        s.append("\n")
    return s

def table_stats(exp, run, ss):
    table = Table(title=f"{exp}: {run}", title_justify="left")
    table_met = Table()
    table_met.add_column("metric")
    counts = []
    row = []
    row_sec = []
    row_l1  = []
    for m in mainfiles:
        if ss[m][' #'] == 0:
            continue
        else:
            counts.append(ss[m][' #'])
        table_met.add_column(f"{m}")
        table.add_column(f"{m} (l1)")
        mn, se = mean_and_stderr(ss[m]["l1"])
        row.append(f"{mn:.3f} ±{se:.3f}")
        row_l1.append(f"{mn:.3f} ±{se:.3f}")
        table.add_column(f"{m} (s)")
        mn, se = mean_and_stderr(ss[m]["ms"])
        mn, se = mn / 1000, se / 1000
        row.append(f"{mn:.3f} ±{se:.3f}")
        row_sec.append(f"{mn:.3f} ±{se:.3f}")

    if any([c != counts[0] for c in counts]):
        print(f"WARNING! counts are not consistent for {exp} {run}! Got:", *counts, "\nfor: ", *mainfiles)
        # return None

    if len(counts) == 0:
        print(f"no valid runs for {exp}: {run} ")
        return None

    print("# runs:", counts[0])
    table.add_row(*row)
    table_met.add_row("l1", *row_l1)
    table_met.add_row("sec", *row_sec)
    return table, table_met

def isdate(x):
    try:
        datetime.strptime(x, "%Y-%m-%d")
        return True
    except ValueError:
        return False
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="average everything in logs/")
    parser.add_argument("--logdir", default="logs/", type=str,)
    experiment_name = os.path.dirname(__file__).split("/")[-1]

    args = parser.parse_args()

    subdirs = list(filter(lambda f: os.path.isdir(args.logdir + f), os.listdir(args.logdir)))
    isdev   = all([isdate(sd) for sd in subdirs])
    experiments = {"bayesnets", "arrival", "grids", "gossip"}
    isfinal = all([sd in experiments for sd in subdirs])

    if not (isdev or isfinal):
        print("ambiguous log folder structure. Expecting <logdir>/[<experiment>/]<%Y-%m-%d>/")
        sys.exit(1)
    ex = experiment_name if isfinal  else ""

    print("logdir", args.logdir)
    print(args.logdir + ex + "/")
    print(list(filter(lambda f: os.path.isdir(args.logdir + ex + "/" + f), os.listdir(args.logdir))))
    for day in filter(lambda f: os.path.isdir(args.logdir + ex + "/" + f), os.listdir(args.logdir)):
        day = args.logdir + "/" + day + "/"
        print("day", day)
        for hm in filter(lambda f: os.path.isdir(day + f), os.listdir(day)):
            run = day + "/" + hm + "/"
            print("run", run)
            stats = {k: {" #":0,"ms":[], "l1":[]} for k in mainfiles}


            for log in filter(lambda l: l[-4:] == ".log", os.listdir(run)):
                main = [m for m in mainfiles if log[:len(m)] == m.replace(".", "-")]
                if len(main) == 0:
                    print(f"warning! Did not find a main file associated with {log}")
                    continue
                else:
                    assert len(main) == 1, f"did not find a main file associated with {log}"
                main = main[0]
                with open(run + "/" + log, "r") as f:
                    out = f.readlines()
                    out = "".join(out).rstrip().split('\n')
                if len(out) == 1 and out[0] == "":
                    print(f"incomplete run: {run}/{log} is empty!")
                    continue
                last = out[-1]
                if last == "":
                    print("warning! last is empty! full output is:")
                    print(out)
                    continue
                if  last[:len("timeout")] == "timeout":
                    continue
                if last[-2:] != "ms":
                    print(f"WARNING! expected {log} to have last line ending in 'ms'")
                    continue
                stats[main]['ms'].append(float(last[:-2]))
                stats[main][' #'] += 1
                if needs_l1(main):
                    out = [float(f) for f in out[-2].rstrip().split()]
                    l1 = sum(compute_l1(out, truth))
                    stats[main]['l1'].append(l1)
                else:
                    stats[main]['l1'].append(0.0)
            if not DEVELOP:
                with open(day + f"/{hm}.stats", "w") as f:
                    f.writelines(str_stats(stats))
            # FIXME replace with logdir?
            table = table_stats(experiment_name, run, stats)
            if table is not None:
                tbl, mets = table
                console = Console(record=True)
                console.print(tbl)
                console.print(mets)
                console.save_text(day + f"/{hm}.stats.rich")

else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
