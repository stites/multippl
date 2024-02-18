import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import torch
import pyro
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
from utils import *


def probfn(i, j):
   match (i, j):
       case (0, 0): return [0.5]
       case (_, 0): return [1./5, 1./6]
       case (0, _): return [1./3, 1./4]
       case (1, _): return [1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0]
       case (2, _): return [2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0]
       case (3, _): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
       case (4, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case (5, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case (6, _): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
       case (7, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case (8, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case _: raise Exception()


if __name__ == "__main__":
    num_samples = 1_000
    num_runs = 2
    start_seed = 0
    args = {}
    sites = ["npackets"]
    truth = [1.7718846855216892]

    model = lambda: mkarrival(9, probfn)

    l1s = []
    times = []
    for run, seedoffset in enumerate(range(1,num_runs+1)):
        seed = start_seed + seedoffset
        print(f"starting run {run+1}/{num_runs} (with seed: {seed})", flush=True)
        torch.manual_seed(seed)
        np.random.seed(seed)
        random.seed(seed)
        start = time.time()

        importance = Importance(model, num_samples=num_samples)
        posterior = importance.run(*args)

        ms = allmarg(posterior, sites, num_samples=num_samples) # don't think need a full
        print("am")
        end = time.time()
        times.append(end - start)
        print(times[-1], "s", flush=True)
        l1 = compute_l1(ms, truth)
        printit(sites, ms, l1)
        l1s.append(sum(l1))
    #return (l1s, times)
