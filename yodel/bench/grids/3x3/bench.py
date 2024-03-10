#!/usr/bin/env python3
import os
import os.path
import sys

from tqdm import tqdm
import subprocess
import time
from multiprocessing import Pool
from subprocess import STDOUT, check_output, CalledProcessError, TimeoutExpired
import signal

USE_NOTI=False
TIMEOUT_SEC = 5
#timeout_sec = 60 * 5 # 5m
repo_dir = subprocess.Popen(['git', 'rev-parse', '--show-toplevel'], stdout=subprocess.PIPE).communicate()[0].rstrip().decode('utf-8')
benchdir = f"{repo_dir}/yodel/bench/"

import threading

class Proc(threading.Thread):
    def __init__(self, run, mainfile, nsteps, nruns, initial_seed, cmd, logdir, with_seed, needs_timer):
        threading.Thread.__init__(self)
        self.date = time.strftime("%Y-%m-%d", time.localtime())
        self.hm  = time.strftime("%H:%M", time.localtime())
        self.start_time = time.time()
        self.seed = run + initial_seed
        self.cmd = cmd if not with_seed else cmd + [str(self.seed)]
        self.mainfile = mainfile
        self.nsteps = nsteps
        self.nruns = nruns
        self.logdir = logdir
        self.needs_timer = needs_timer
        self.outfilepath = logdir + "_".join([
            f"{self.mainfile.replace('.', '-')}",
            f"n{self.nsteps}",
            f"s{self.seed}",
            f"{self.date}_{self.hm}.log"])

    def run(self):
        self.p = subprocess.Popen(self.cmd) # timeout=TIMEOUT_SEC,
        self.p.wait()
        # print("running")
        with open(self.outfilepath, "w") as outfile:
            self.p = subprocess.Popen(self.cmd, stdout=outfile) # timeout=TIMEOUT_SEC,
            if self.needs_timer:
                outfile.write(f"{self.timeit()}ms\n")
            self.p.wait()

    def timeit(self):
        return time.time() - self.start_time

    def run_with_timeout(self, timeout):
        self.start()
        self.join(timeout)
        if self.is_alive():
            print("terminating")
            #self.p.terminate()      #use self.p.kill() if process needs a kill -9
            self.p.kill()      #use self.p.kill() if process needs a kill -9
            with open(self.outfilepath, "w") as outfile:
                outfile.write(f"timeout@{timeout} (seconds)\n")
            self.join()
        else:
            if self.run == 0:
                Proc.noti_success(self.mainfile, 1, self.nruns, self.timeit())
            if self.p.returncode > 0:
                with open(self.outfilepath, "w") as outfile:
                    outfile.write(f"error@{self.timeit()} (milliseconds)\n")
                Proc.noti_failed(self.mainfile, self.p.returncode, self.run, self.nruns)

    @staticmethod
    def _noti(title, message):
        if USE_NOTI:
            import subprocess, os
            env = os.environ.copy()
            subprocess.run(["noti", "-o", "-t", f"\"{title}\"", '-m', f'"{message}"'], env=env)
        else:
            print(title, ":", message)

    @staticmethod
    def noti_failed(mainfile, exitcode, run_ix, num_runs, timeout=0.0):
        title = f"{mainfile} ({run_ix} / {num_runs}): {exitcode}"
        if timeout > 0:
            message = f"timeout after {timeout}s"
        else:
            message = "failed!"
        Proc._noti(title, message)

    @staticmethod
    def noti_success(mainfile, run_ix, num_runs, sec_per_run):
        title = f"{mainfile} ({run_ix} / {num_runs})"
        message = "done! @ {:.2f}s".format(sec_per_run)
        Proc._noti(title, message)


def runner_(mainfile, cmd, with_seed=True, logdir="logs/", **args):
    nsteps = args['num_steps']
    nruns = args['num_runs']

    start = time.time()
    all_args = [(run, mainfile, nsteps, nruns, args['initial_seed'], cmd, logdir, with_seed, args['needs_timer'],) for run in range(nruns)]
    pbar = tqdm([Proc(*args).run_with_timeout(TIMEOUT_SEC) for args in all_args], total=nruns)

    # with Pool(nruns) as p:
    #     pbar = tqdm(p.imap_unordered(Proc, all_args), total=nruns)
    #     pbar.set_description(mainfile + f"(n:{nsteps})")
    #     list(pbar)
    end = time.time()
    Proc.noti_success(mainfile, nruns, nruns,  (end - start) / nruns)

def pyrunner(mainfile, logdir="logs/", **args):
    cmd = ["python", mainfile, "--num-samples", str(args['num_steps']), "--num-runs", "1", "--seed"]
    runner_(mainfile, cmd, with_seed=True, logdir=logdir, needs_timer=False, **args)

def timedrunner(bin, mainfile, logdir="logs/", **args):
    cmd = [bin, mainfile]
    runner_(mainfile, cmd, with_seed=False, logdir=logdir, needs_timer=True, **args)

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
        runner_(mainfile, cmd, with_seed=True, logdir=logdir, needs_timer=False, **args)

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
