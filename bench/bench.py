#!/usr/bin/env python3
import os
import os.path
import sys
import multiprocessing
import shutil


from tqdm import tqdm, trange
import subprocess
import time
from multiprocessing import Pool
import multiprocessing.context as ctx

DEVELOP = False
# USE_NOTI=False if DEVELOP else True
USE_NOTI = False
TIMEOUT_SEC = 20 if DEVELOP else 10 * 60  # = 30min

def mkoutpath(logdir, mainfile, nsteps, seed, date, hm):
    outfilepath = logdir + "_".join(
        [f"{mainfile.replace('.', '-')}", f"n{nsteps}", f"s{seed}", f"{date}_{hm}.log"]
    )
    return outfilepath


def proc(args):
    run, mainfile, nsteps, nruns, initial_seed, cmd, logdir, with_seed, needs_timer = (
        args
    )
    date = time.strftime("%Y-%m-%d", time.localtime())
    hm = time.strftime("%H:%M", time.localtime())
    seed = run + initial_seed
    outfilepath = mkoutpath(logdir, mainfile, nsteps, seed, date, hm)

    start = time.time()
    cmd = cmd if not with_seed else cmd + [str(seed)]
    try:
        with open(outfilepath, "w") as outfile:
            p = subprocess.run(cmd, stdout=outfile, stderr=subprocess.DEVNULL, timeout=TIMEOUT_SEC)
            if needs_timer and p.returncode == 0:
                sec = time.time() - start
                outfile.write(f"{sec * 1000}ms\n")

        if p.returncode > 0:
            os.remove(outfilepath)
            noti_failed(mainfile, p.returncode, run, nruns)

        if run == 0 and p.returncode == 0:
            end1 = time.time()
            noti_success(mainfile, 1, nruns, (end1 - start))



    except (subprocess.TimeoutExpired, ctx.TimeoutError):
        with open(outfilepath, "w") as outfile:
            outfile.write(f"timeout@{TIMEOUT_SEC} (seconds)\n")
        noti_failed(mainfile, 124, run, nruns)


def runner_(mainfile, cmd, with_seed=True, logdir="logs/", needs_timer=False, **kwargs):
    nsteps = kwargs["num_steps"]
    nruns = kwargs["num_runs"]
    nthreads = kwargs["threads"]
    iseed = kwargs["initial_seed"]

    start = time.time()
    all_args = [
        (run, mainfile, nsteps, nruns, iseed, cmd, logdir, with_seed, needs_timer)
        for run in range(nruns)
    ]
    with Pool(nthreads) as p:
        pbar = tqdm(p.imap_unordered(proc, all_args), total=nruns)
        pbar.set_description(mainfile + f"(n:{nsteps})")
        list(pbar)
    end = time.time()
    noti_success(mainfile, nruns, nruns, (end - start) / nruns)


def _noti(title, message):
    if USE_NOTI:
        import subprocess, os

        env = os.environ.copy()
        subprocess.run(
            ["noti", "-o", "-t", f'"{title}"', "-m", f'"{message}"'], env=env
        )
    else:
        # print(title, ":", message)
        pass


def noti_failed(mainfile, exitcode, run_ix, num_runs):
    title = f"{mainfile} ({run_ix} / {num_runs}): {exitcode}"
    message = "failed!"
    _noti(title, message)


def noti_success(mainfile, run_ix, num_runs, sec_per_run):
    title = f"{mainfile} ({run_ix} / {num_runs})"
    message = "done! @ {:.2f}s".format(sec_per_run)
    _noti(title, message)


def pyrunner(mainfile, logdir="logs/", **kwargs):
    # cmd = ["python", mainfile, "--num-samples", str(kwargs['num_steps']), "--num-runs", "1", "--seed"]
    cmd = ["python", mainfile, "--num-samples", str(kwargs["num_steps"]), "--seed"]
    runner_(mainfile, cmd, with_seed=True, logdir=logdir, needs_timer=False, **kwargs)


def timedrunner(bin, mainfile, logdir="logs/", **kwargs):
    # cmd = [benchdir + "time.sh", bin, mainfile]
    extracli = kwargs["extracli"] if "extracli" in kwargs else []
    cmd = [bin, *extracli, mainfile]
    runner_(mainfile, cmd, with_seed=False, logdir=logdir, needs_timer=True, **kwargs)


def yorunner(mainfile, logdir="logs/", **kwargs):
    multipplbin = shutil.which("multippl")
    if multipplbin is None:
        subprocess.run(
            ["cargo", "build", "--release", "--bin", "multippl"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        repo_dir = (
           subprocess.Popen(["git", "rev-parse", "--show-toplevel"], stdout=subprocess.PIPE)
           .communicate()[0]
           .rstrip()
           .decode("utf-8")
          )
        multipplbin = f"{repo_dir}/target/release/multippl"

    if not os.path.isfile(multipplbin):
        raise Exception("multippl binary was not built correctly")
    else:
        multipplcmd = [multipplbin]
        filearg = ["--file", mainfile]
        dataarg = ["--data", "data.json"] if os.path.isfile("data.json") else []
        nsteps = ["--steps", str(kwargs["num_steps"])]
        cmd = multipplcmd + filearg + dataarg + nsteps + ["--rng"]
        runner_(
            mainfile, cmd, with_seed=True, logdir=logdir, needs_timer=False, **kwargs
        )


if __name__ == "__main__":
    import sys
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--psi", default=False, action="store_true")
    parser.add_argument(
        "--num-runs",
        default=100,
        type=int,
    )
    parser.add_argument(
        "--num-steps",
        default=1_000,
        type=int,
    )
    parser.add_argument(
        "--initial-seed",
        default=0,
        type=int,
    )
    parser.add_argument("--noti", default=False, action="store_true")
    parser.add_argument(
        "--threads",
        default=multiprocessing.cpu_count() // 2,
        type=int,
    )
    parser.add_argument(
        "--logdir",
        default="logs/",
        type=str,
    )
    args = parser.parse_args()

    path = os.getcwd()
    parentpath = os.path.abspath(os.path.join(path, os.pardir))
    exp_dir = os.path.abspath(os.getcwd()).split("/")[-2:]
    if exp_dir[0] not in {"gossip", "grids", "arrival", "bayesnets"}:
        print("Run bench.py in an experiment folder: <gossip|grids|arrival|bayesnets>/<experiment_dir>/", file=sys.stderr)
        sys.exit(1)

    exp_dir = "/".join(exp_dir)
    date = time.strftime("%Y-%m-%d", time.localtime())
    hm = time.strftime("%H:%M", time.localtime())
    logdir = "/".join([args.logdir, exp_dir, date, hm]) + "/"

    print("logdir =", logdir)
    args.logdir = logdir
    os.makedirs(logdir, exist_ok=True)
    reserved = [
        "bench.py",
        "avg.py",
        "utils.py",
        "stdin2l1.py",
        "truth.py",
        "truth.sh",
        "main.dice-partial",
    ]

    files = [f for f in os.listdir(".") if os.path.isfile(f) and not (f in reserved)]

    args = vars(args)
    num_steps = args["num_steps"]
    for f in files:
        if args["psi"] and f[-4:] == ".psi":
            args["num_steps"] = 1
            timedrunner("psi", f, extracli=[], **args)
            args["num_steps"] = num_steps
            pass
        elif not args["psi"]:
            if f[-3:] == ".py":
                pyrunner(f, **args)
                pass
            elif f[-5:] == ".dice":
                args["num_steps"] = 1
                timedrunner("dice", f, **args)
                args["num_steps"] = num_steps
                pass
            elif (
                (f[:5] == "grids" and f[-3:] == ".yo" and len(f) == 11)
                or (  # grids#x#.yo
                    f[:5] == "grids" and f[-9:] == "-obs01.yo" and len(f) == 17
                )
                or (f == "exact.yo" or f == "disc.yo")  # grids#x#-obs01.yo
            ):  # disc programs
                # we are compiling exactly, only use one sample
                isexact = (
                    (f[:5] == "grids" and f[-3:] == ".yo" and len(f) == 11)
                    or (  # grids#x#.yo
                        f[:5] == "grids" and f[-9:] == "-obs01.yo" and len(f) == 17
                    )
                    or (f == "exact.yo" or f == "disc.yo")  # grids#x#-obs01.yo
                )
                notpsi = not args["psi"]
                args["num_steps"] = 1

                yorunner(f, **args)
                args["num_steps"] = num_steps
                pass
            elif f[-3:] == ".yo" and not args["psi"]:
                # if "hbn" in parentpath:
                #    args["num_steps"] = 100
                yorunner(f, **args)
                # if "hbn" in parentpath:
                #    args["num_steps"] = num_steps
                pass
            else:
                print(f"WARNING! saw unexpected file {f}")
        else:
            print(f"WARNING! saw unexpected file {f}")

else:
    import torch

    def ismean(em):
        keepdim = False
        value = em._samples
        weights = em._log_weights.reshape(
            em._log_weights.size()
            + torch.Size([1] * (value.dim() - em._log_weights.dim()))
        )
        dim = em._aggregation_dim
        probs = weights.exp()
        return (value * probs).sum(dim=dim, keepdim=keepdim) / probs.sum(
            dim=dim, keepdim=keepdim
        )
