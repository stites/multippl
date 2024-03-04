#!/usr/bin/env python3
import os
import os.path
import sys

from tqdm import tqdm
import subprocess
import time
from multiprocessing import Pool

repo_dir = subprocess.Popen(['git', 'rev-parse', '--show-toplevel'], stdout=subprocess.PIPE).communicate()[0].rstrip().decode('utf-8')
benchdir = f"{repo_dir}/yodel/bench/"

def proc(args):
    run, mainfile, nsteps, nruns, initial_seed, cmd, logdir, with_seed = args
    date = time.strftime("%Y-%m-%d", time.localtime())
    hm  = time.strftime("%H:%M", time.localtime())

    outfilepath = lambda seed: logdir + "_".join([
        f"{mainfile.replace('.', '-')}",
        f"n{nsteps}",
        f"s{seed}",
        f"{date}_{hm}.log"])


    start = time.time()
    seed = run + initial_seed
    cmd = cmd if not with_seed else cmd + [str(seed)]
    with open(outfilepath(seed), "w") as outfile:
        p = subprocess.run(cmd, stdout=outfile)

    if run == 0 and p.returncode == 0:
        end1 = time.time()
        noti_success(mainfile, 1, nruns, (end1 - start))

    if p.returncode > 0:
        noti_failed(mainfile, p.returncode, run, nruns)

def runner_(mainfile, cmd, with_seed=True, logdir="logs/",**args):
    nsteps = args['num_steps']
    nruns = args['num_runs']

    start = time.time()
    all_args = [(run, mainfile, nsteps, nruns, args['initial_seed'], cmd, logdir, with_seed) for run in range(nruns)]
    with Pool(nruns) as p:
        pbar = tqdm(p.imap(proc, all_args), total=nruns)
        pbar.set_description(mainfile + f"(n:{nsteps})")
        list(pbar)
    end = time.time()
    noti_success(mainfile, nruns, nruns,  (end - start) / nruns)

def _noti(title, message):
    import subprocess, os
    env = os.environ.copy()
    subprocess.run(["noti", "-o", "-t", f"\"{title}\"", '-m', f'"{message}"'], env=env)

def noti_failed(mainfile, exitcode, run_ix, num_runs):
    title = f"{mainfile} ({run_ix} / {num_runs}): {exitcode}"
    message = "failed!"
    _noti(title, message)

def noti_success(mainfile, run_ix, num_runs, sec_per_run):
    title = f"{mainfile} ({run_ix} / {num_runs})"
    message = "done! @ {:.2f}s".format(sec_per_run)
    _noti(title, message)

def pyrunner(mainfile, logdir="logs/", **args):
    cmd = ["python", mainfile, "--num-samples", str(args['num_steps']), "--num-runs", "1", "--seed"]
    runner_(mainfile, cmd, with_seed=True, logdir=logdir, **args)

def timedrunner(bin, mainfile, logdir="logs/", **args):
    cmd = [benchdir + "time.sh", bin, mainfile]
    runner_(mainfile, cmd, with_seed=False, logdir=logdir, **args)

def yorunner(mainfile, logdir="logs/", **args):
    subprocess.run(["cargo", "build", "--release", "--bin", "yodel"],
                   stdout=subprocess.DEVNULL,
                   stderr=subprocess.DEVNULL,
                   )
    yodelbin = f"{repo_dir}/target/release/yodel"
    if not os.path.isfile(yodelbin):
        raise Exception("yodel binary was not built correctly")
    else:
        yodelcmd = [yodelbin]
        filearg = ["--file", mainfile]
        dataarg = ["--data", "data.json"] if os.path.isfile("data.json") else []
        nsteps = ["--steps", str(args['num_steps'])]
        cmd = yodelcmd + filearg + dataarg + nsteps + [ "--rng" ]
        runner_(mainfile, cmd, with_seed=True, logdir=logdir, **args)

if __name__ == "__main__":
    import sys
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-runs", default=10, type=int,)
    parser.add_argument("--num-steps", default=1_000, type=int,)
    parser.add_argument("--initial-seed", default=0, type=int,)
    parser.add_argument("--noti", default=True, type=bool,)
    parser.add_argument("--out-dir", default="logs/", type=str,)
    args = parser.parse_args()

    date = time.strftime("%Y-%m-%d", time.localtime())
    hm  = time.strftime("%H:%M", time.localtime())
    logdir = args.out_dir + date + "/" + hm + "/"
    os.makedirs(logdir, exist_ok=True)
    reserved = ["bench.py", "avg.py"]
    files = [f for f in os.listdir('.') if os.path.isfile(f) and not (f in reserved)]

    args = vars(args)
    num_steps = args["num_steps"]
    for f in files:
        if f[-3:] == ".py":
            pyrunner(f, logdir=logdir, **args)
            pass
        elif f[-5:] == ".dice":
            timedrunner("dice", f, logdir=logdir, **args)
            pass
        elif f[-4:] == ".psi":
            timedrunner("psi", f, logdir=logdir, **args)
            pass
        elif f[:5] == "grids" and f[-3:] == ".yo" and len(f) == 11: # we are compiling exactly, only use one sample
            args["num_steps"] = 1
            yorunner(f, logdir=logdir, **args)
            args["num_steps"] = num_steps
            pass
        elif f[-3:] == ".yo":
            yorunner(f, logdir=logdir, **args)
            pass
        else:
            print(f"WARNING! saw unexpected file {f}")

else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
