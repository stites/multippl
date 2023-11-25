import torch
torch.set_num_threads(1)
import pyro
import numpy as np
import random
from pyro import poutine
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
from pyro.infer import config_enumerate
import time

def flip(n, p, suffix=""):
    return pyro.sample("flip"+n+suffix, dist.Bernoulli(p))

def edge(n, parent, t, f, suffix=""):
    return flip(n, t, suffix=suffix) if parent == 1 else flip(n, f, suffix=suffix)

def node(n, p0, p1, tt, tf, ft, ff, suffix=""):
    if p0 == 1 and p1 == 1:
        return flip(n, tt, suffix=suffix)
    elif p0 == 1 and p1 == 0:
        return flip(n, tf, suffix=suffix)
    elif p0 == 0 and p1 == 1:
        return flip(n, ft, suffix=suffix)
    else:
        return flip(n, tt, suffix=suffix)

def mkgrid(n, probfn, **kwargs):
    def mk(i, j, *args, **kwargs):
       match (i, j):
           case (0, 0):
               return flip("00", *args, **kwargs)
           case (0, _):
               return edge("0"+str(j), *args, **kwargs)
           case (_, 0):
               return edge(str(i)+"0", *args, **kwargs)
           case _:
               return node(str(i)+str(j), *args, **kwargs)
    def parents(g, i, j):
       match (i, j):
           case (0, 0):
               return []
           case (0, _):
               return [g[0][i-1]]
           case (_, 0):
               return [g[i-1][0]]
           case _:
               return [g[i][j-1], g[i-1][j]]
    grid = []
    g = torch.empty(n,n)
    for i in range(0,n):
        grid.append([])
        for j in range(0,n):
            grid[i].append(mk(i, j, *parents(grid, i, j), *probfn(i, j), **kwargs))

        g[i] = torch.stack(grid[i])
    return g

def mkarrival(n, probfn):
    npackets = pyro.sample("npackets", dist.Poisson(3))
    ss = torch.empty(n,n)
    for ix in pyro.plate("packet", int(npackets.item())+1):
        ss += mkgrid(n, probfn, suffix="_"+str(ix))
    return ss / n

def arrival_sites(sites):
    return [s + "_0" for s in sites]


def allmarg(posterior, sites, num_samples):
    marginal = EmpiricalMarginal(posterior, sites = sites)
    tensor = torch.sum(torch.cat([marginal().unsqueeze(0) for _ in range(num_samples)]), dim=0) / num_samples
    return [t.item() for t in tensor]

def compute_l1(ms, truth):
    return list(map(lambda x: abs(x[0] - x[1]), zip(ms, truth)))

def printit(sites, ms, l1s):
    for d, t, l1 in zip(sites, ms, l1s):
        print(f"P({d})={t:.6f}  (Δ: {l1:.6f})")
    print("L1: ", sum(l1s), flush=True)

def runall(model, sites, truth, num_runs, num_samples):
    l1s = []
    times = []
    for run, seed in enumerate(range(1,num_runs+1)):
        print(f"starting run {run+1}/{num_runs} (with seed: {seed})", flush=True)
        torch.manual_seed(seed)
        np.random.seed(seed)
        random.seed(seed)
        start = time.time()

        importance = Importance(model, num_samples=num_samples)
        posterior = importance.run()

        end = time.time()
        times.append(end - start)
        print(times[-1], "s", flush=True)
        ms = allmarg(posterior, sites, num_samples=int(num_samples / 10) if num_samples > 1000 else num_samples) # don't think wneed a full
        l1 = compute_l1(ms, truth)
        printit(sites, ms, l1)
        l1s.append(sum(l1))
    return (l1s, times)
