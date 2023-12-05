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

    grids3x3_diag = "grids3x3-diag"
    grids6x6_diag = "grids6x6-diag"
    grids9x9_diag = "grids9x9-diag"

    grids3x3_sample = "grids3x3-sample"
    grids6x6_sample = "grids6x6-sample"
    grids9x9_sample = "grids9x9-sample"

    hmm = "hmm"
    hybrid0 = "hybrid0"
    beta_bernoulli = "beta-bernoulli"
    all = "all"

    def __str__(self):
        return self.value

class Backend(Enum):
    python = "python"
    yodel = "yodel"
    dice = "dice"
    #psi = "psi"
    all = "all"
    def __str__(self):
        return self.value

def gridwd(ex:Experiment):
    return "grids/" + f"{ex}"[5:8]

def gridmain(ex:Experiment, b: Backend):
    if f"{ex}".startswith("grids"):
        match b:
            case Backend.python:
                return f"{ex}"[:8] + ".py"
            case Backend.yodel:
                return f"{ex}.yo"
            case Backend.dice:
                return f"{ex}"[:8] + ".dice"
            case x:
                print("backend should be one of {}. Got: {}".format(list(Backend), x), file=sys.stderr)
                sys.exit(1)


def files(ex:Experiment, b: Backend):
    wd = gridwd(ex) if f"{ex}".startswith("grids") else f"{ex}"
    mainpy = gridmain(ex, b) if f"{ex}".startswith("grids") else "main.py"
    mainyo = gridmain(ex, b) if f"{ex}".startswith("grids") else "main.yo"
    maindice = gridmain(ex, b) if f"{ex}".startswith("grids") else None
    return (wd, mainpy, mainyo, maindice)

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
            p = subprocess.run(cmd(seed), stdout=outfile, cwd = files(args['experiment'], args['type'])[0])
        noti(p.returncode, run, **args)

def noti(exitcode, rix, experiment, type, num_runs, **args):
    import subprocess, os
    env = os.environ.copy()
    subprocess.run(["noti", "-o", '-e', "-t", f"\"{experiment} ({type}: {rix+1}/{num_runs}): {exitcode}\"", '-m', '"done"' if exitcode == 0 else '"failed!"'], env=env)

def pyrunner(**args):
    (_, main, _, _) = files(args['experiment'], args['type'])
    cmd = lambda seed: ["python", main, "--num-samples", str(args['num_steps']), "--seed", str(seed)] + ([ "--num-runs", "1" ] if f"{args['experiment']}".startswith("grids") else [])
    runner_(cmd, **args)

def dicerunner(**args):
    (wd, _, _, main) = files(args['experiment'], args['type'])
    benchdir = "/".join(map(lambda x: '..', wd.split("/"))) + "/"
    cmd = lambda _: [benchdir + "time.sh", "dice", main]
    runner_(cmd, **args)

def yorunner(**args):
    subprocess.run(["cargo", "build", "--release"])
    yodelbin = f"{repo_dir}/target/release/yodel"
    if not os.path.isfile(yodelbin):
        raise Exception("yodel binary was not built correctly")
    else:
        yodelcmd = [yodelbin]
        (_, _, main, _) = files(args['experiment'], args['type'])
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
        case Backend.dice:
            dicerunner(**args)
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
    parser.add_argument("--num-runs", default=10, type=int,)
    parser.add_argument("--num-steps", default=1_000, type=int,)
    parser.add_argument("--initial-seed", default=0, type=int,)
    parser.add_argument("--noti", default=True, type=bool,)
    parser.add_argument("--out-dir", default="out/", type=str,)
    parser.add_argument("--skip", type=str)
    args = parser.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    match args.experiment:
        case Experiment.all: # TODO
            d = vars(args)
            skip = [Experiment.all]
            if args.skip is not None:
                skip += [Experiment[s.strip()] for s in args.skip.split(",")]
            for ex in list(Experiment):
                if ex not in skip:
                    d["experiment"] = ex
                    print(f"running {d['experiment']}")
                    runner(**d)
        case x:
            runner(**vars(args))
else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
