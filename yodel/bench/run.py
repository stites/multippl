#!/usr/bin/env python3
import os
import os.path
import sys

from tqdm import tqdm
import subprocess
from argparse import ArgumentParser
from enum import Enum
from datetime import datetime
import time

repo_dir = subprocess.Popen(['git', 'rev-parse', '--show-toplevel'], stdout=subprocess.PIPE).communicate()[0].rstrip().decode('utf-8')

class Experiment(Enum):
    grids3x3 = "grids3x3"
    grids6x6 = "grids6x6"
    grids9x9 = "grids9x9"
    hmm = "hmm"
    hybrid0 = "hybrid0"
    beta_bernoulli = "beta-bernoulli"
    all = "all"

    def __str__(self):
        return self.value

class Backend(Enum):
    python = "python"
    yodel = "yodel"
    all = "all"
    def __str__(self):
        return self.value

def files(ex:Experiment):
    wd = "grids" if f"{ex}".startswith("grids") else f"{ex}"
    mainpy = f"{ex}.py" if f"{ex}".startswith("grids") else "main.py"
    mainyo = f"{ex}-sampled.yo" if f"{ex}".startswith("grids") else "main.yo"
    return (wd, mainpy, mainyo)


def runner_(cmd, **args):
    dt = time.strftime("%Y-%m-%d_%H:%M", time.localtime())
    nsteps = args['num_steps']
    outfilepath = lambda seed: "_".join([
        args['out_dir'] + f"{args['experiment']}",
        dt,
        f"s{seed}",
        f"n{nsteps}",
        f"{args['type']}.log"])
    for run in tqdm(range(args['num_runs'])):
        seed = run + args['initial_seed']
        with open(outfilepath(seed), "w") as outfile:
            subprocess.run(cmd(seed), stdout=outfile, cwd = files(args['experiment'])[0])


def pyrunner(**args):
    (_, main, _) = files(args['experiment'])
    cmd = lambda seed: ["python", main, "--num-samples", str(args['num_steps']), "--seed", str(seed)] + ([ "--num-runs", "1" ] if f"{args['experiment']}".startswith("grids") else [])
    runner_(cmd, **args)

def yorunner(**args):
    subprocess.run(["cargo", "build", "--release"])
    yodelbin = f"{repo_dir}/target/release/yodel"
    if not os.path.isfile(yodelbin):
        raise Exception("yodel binary was not built correctly")
    else:
        yodelcmd = [yodelbin]
        (_, _, main) = files(args['experiment'])
        filearg = ["--file", main]
        datafile = f"data.json"
        dataarg = ["--data", datafile] if os.path.isfile(f"{args['experiment']}/{datafile}") else []
        cmd = lambda seed: yodelcmd + filearg + dataarg + ["--steps", str(args['num_steps']), "--rng", str(seed)]
        runner_(cmd, **args)

def runner(**args):
    match args['type']:
        case Backend.python:
            pyrunner(**args)
        case Backend.yodel:
            yorunner(**args)
        case Backend.all:
            d = {k: v for k, v in args.items()}
            print("running python...")
            d['type'] = Backend.python
            pyrunner(**d)

            print("running yodel...")
            d['type'] = Backend.yodel
            yorunner(**d)
        case x:
            print("backend required, one of {}.\n'{}' is not valid".format(list(Backend), x), file=sys.stderr);
            sys.exit(1)

if __name__ == "__main__":
    import sys
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("experiment", type=Experiment, choices=list(Experiment))
    parser.add_argument("--type", type=Backend, choices=list(Backend))
    parser.add_argument("--num-runs", default=2, type=int,)
    parser.add_argument("--num-steps", default=10, type=int,)
    parser.add_argument("--initial-seed", default=0, type=int,)
    parser.add_argument("--out-dir", default="out/", type=str,)
    args = parser.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    match args.experiment:
        case Experiment.beta_bernoulli:
            runner(**vars(args))
        case Experiment.grids3x3:
            runner(**vars(args))
        case Experiment.grids6x6:
            runner(**vars(args))
        case Experiment.grids9x9:
            runner(**vars(args))
        case Experiment.hmm:
            runner(**vars(args))
        case Experiment.hybrid0:
            runner(**vars(args))
        case Experiment.all: # TODO
            d = vars(args)
            for ex in list(Experiment):
                if ex is not Experiment.all:
                    d["experiment"] = ex
                    print(f"running {d['experiment']}")
                    runner(**d)
        case x:
            print("experiment '{}' is not added to this CLI".format(x), file=sys.stderr);
            sys.exit(1)
else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
