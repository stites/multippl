#!/usr/bin/env python3
import argparse
import time
import os, sys
import numpy as np
import scipy.stats
from rich.console import Console
from rich.table import Table
from datetime import datetime
from collections import namedtuple
import pickle
import json

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

#Entry = namedtuple('Entry', ['mean', 'stderr'])
def mean_and_stderr(data):
    a = 1.0 * np.array(data)
    return dict(mean=np.mean(a), stderr=scipy.stats.sem(a))

def mn_se_as_tuple(d):
    return d['mean'], d['stderr']

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
                mean, stderr = mn_se_as_tuple(mean_and_stderr(ss[m][metric]))
                s.append("{} {}: {} ±{}".format(m.ljust(pad, " "), metric, str(mean).ljust(pad+10, " "), str(stderr)))
            s.append("\n")
        s.append("\n")
    return s


#Eval = namedtuple('Eval', ['l1', 'seconds', 'counts'])

def table_stats(exp, run, ss):
    table = Table(title=f"{exp}: {run}", title_justify="left")
    table_met = Table()
    table_met.add_column("metric")
    counts = []
    row = []
    row_sec = []
    row_l1  = []
    raw = dict()
    for m in mainfiles:
        raw[m] = dict()
        if ss[m][' #'] == 0:
            continue
        else:
            counts.append(ss[m][' #'])
        table_met.add_column(f"{m}")
        table.add_column(f"{m} (l1)")
        l1_mn_se = mean_and_stderr(ss[m]["l1"])
        mn, se = mn_se_as_tuple(l1_mn_se)
        raw[m]["s"] = mn
        row.append(f"{mn:.3f} ±{se:.3f}")

        row_l1.append(f"{mn:.3f} ±{se:.3f}")
        table.add_column(f"{m} (s)")
        sec_mn_se = mean_and_stderr(list(map(lambda x : x / 1000, ss[m]["ms"])))
        mn, se = mn_se_as_tuple(sec_mn_se)
        row.append(f"{mn:.3f} ±{se:.3f}")
        row_sec.append(f"{mn:.3f} ±{se:.3f}")
        raw[m] = dict(l1=l1_mn_se, seconds=sec_mn_se, counts=ss[m][' #'])

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
    return table, table_met, raw

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

    exp_dir = os.path.abspath(os.getcwd()).split("/")[-2:]
    if exp_dir[0] not in {"gossip", "grids", "arrival", "bayesnets"}:
        print("Run avg.py in an experiment folder: <gossip|grids|arrival|bayesnets>/<experiment_dir>/", file=sys.stderr)
        sys.exit(1)
    exp_dir = "/".join(exp_dir) + "/"

    ex = exp_dir

    logdir = args.logdir + "/" + ex
    print("logdir =", logdir)
    # print(list(filter(lambda f: os.path.isdir(args.logdir + ex + f), os.listdir(args.logdir))))

    for day in filter(lambda f: os.path.isdir(logdir + f), os.listdir(logdir)):
        day = logdir + "/" + day + "/"
        for hm in filter(lambda f: os.path.isdir(day + f), os.listdir(day)):
            run = day + "/" + hm + "/"
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
            table = table_stats(experiment_name, run, stats)
            if table is not None:
                tbl, mets, raw = table

                console = Console(record=True)
                console.print(tbl)
                console.save_text(f"{day}/{hm}.row.rich")

                console = Console(record=True)
                console.print(mets)
                console.save_text(f"{day}/{hm}.stats.rich")
                with open(f"{day}/{hm}.data.json", "w") as fp:
                    json.dump(raw, fp)

else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
