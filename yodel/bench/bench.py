#!/usr/bin/env python3
import os
import os.path
import sys

from tqdm import tqdm
import subprocess
import time
from multiprocessing import Pool
import multiprocessing.context as ctx

DEVELOP=False
USE_NOTI=False if DEVELOP else True
TIMEOUT_SEC= 20 if DEVELOP else 10 * 60 # = 30min
repo_dir = subprocess.Popen(['git', 'rev-parse', '--show-toplevel'], stdout=subprocess.PIPE).communicate()[0].rstrip().decode('utf-8')
benchdir = f"{repo_dir}/yodel/bench/"

def mkoutpath(logdir, mainfile, nsteps, seed, date, hm):
    outfilepath = logdir + "_".join([
        f"{mainfile.replace('.', '-')}",
        f"n{nsteps}",
        f"s{seed}",
        f"{date}_{hm}.log"])
    return outfilepath


def proc(args):
    run, mainfile, nsteps, nruns, initial_seed, cmd, logdir, with_seed, needs_timer = args
    date = time.strftime("%Y-%m-%d", time.localtime())
    hm  = time.strftime("%H:%M", time.localtime())
    seed = run + initial_seed
    outfilepath = mkoutpath(logdir, mainfile, nsteps, seed, date, hm)

    start = time.time()
    cmd = cmd if not with_seed else cmd + [str(seed)]
    try:
        with open(outfilepath, "w") as outfile:
            p = subprocess.run(cmd, stdout=outfile, timeout=TIMEOUT_SEC)
            if needs_timer:
                sec = time.time() - start
                outfile.write(f"{sec * 1000}ms\n")

        if run == 0 and p.returncode == 0:
            end1 = time.time()
            noti_success(mainfile, 1, nruns, (end1 - start))

        if p.returncode > 0:
            noti_failed(mainfile, p.returncode, run, nruns)
    except (subprocess.TimeoutExpired, ctx.TimeoutError):
        with open(outfilepath, "w") as outfile:
            outfile.write(f"timeout@{TIMEOUT_SEC} (seconds)\n")
        noti_failed(mainfile, 124, run, nruns)

def runner_(mainfile, cmd, with_seed=True, logdir="logs/", needs_timer=False, **kwargs):
    nsteps = kwargs['num_steps']
    nruns = kwargs['num_runs']
    iseed = kwargs['initial_seed']

    start = time.time()
    all_args = [(run, mainfile, nsteps, nruns, iseed, cmd, logdir, with_seed, needs_timer) for run in range(nruns)]
    with Pool(nruns) as p:
        pbar = tqdm(p.imap_unordered(proc, all_args), total=nruns)
        pbar.set_description(mainfile + f"(n:{nsteps})")
        list(pbar)
    end = time.time()
    noti_success(mainfile, nruns, nruns,  (end - start) / nruns)

def _noti(title, message):
    if USE_NOTI:
        import subprocess, os
        env = os.environ.copy()
        subprocess.run(["noti", "-o", "-t", f"\"{title}\"", '-m', f'"{message}"'], env=env)
    else:
        print(title, ":", message)

def noti_failed(mainfile, exitcode, run_ix, num_runs):
    title = f"{mainfile} ({run_ix} / {num_runs}): {exitcode}"
    message = "failed!"
    _noti(title, message)

def noti_success(mainfile, run_ix, num_runs, sec_per_run):
    title = f"{mainfile} ({run_ix} / {num_runs})"
    message = "done! @ {:.2f}s".format(sec_per_run)
    _noti(title, message)

def pyrunner(mainfile, logdir="logs/", **kwargs):
    cmd = ["python", mainfile, "--num-samples", str(kwargs['num_steps']), "--num-runs", "1", "--seed"]
    runner_(mainfile, cmd, with_seed=True, logdir=logdir, needs_timer=False, **kwargs)

def timedrunner(bin, mainfile, logdir="logs/", **kwargs):
    # cmd = [benchdir + "time.sh", bin, mainfile]
    cmd = [bin, mainfile]
    runner_(mainfile, cmd, with_seed=False, logdir=logdir, needs_timer=True, **kwargs)

def yorunner(mainfile, logdir="logs/", **kwargs):
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
        nsteps = ["--steps", str(kwargs['num_steps'])]
        cmd = yodelcmd + filearg + dataarg + nsteps + [ "--rng" ]
        runner_(mainfile, cmd, with_seed=True, logdir=logdir, needs_timer=False, **kwargs)

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
            args["num_steps"] = 1
            timedrunner("dice", f, logdir=logdir, **args)
            args["num_steps"] = num_steps
            pass
        elif f[-4:] == ".psi":
            args["num_steps"] = 1
            timedrunner("psi", f, logdir=logdir, **args)
            args["num_steps"] = num_steps
            pass
        elif (f[:5] == "grids" and f[-3:] == ".yo" and len(f) == 11) or (   # grids#x#.yo
              f[:5] == "grids" and f[-9:] == "-obs01.yo" and len(f) == 17): # grids#x#-obs01.yo
            # we are compiling exactly, only use one sample
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
