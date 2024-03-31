import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import torch
import pyro
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
from utils import *

def network(suffix=""):
    def flip(n, p):
        return pyro.sample(n+suffix, dist.Bernoulli(p))
    n40r = True
    n30r = flip("n30r", 1.0 / 2.0) if     n40r else torch.zeros(1)
    n41r = flip("n41r", 1.0 / 2.0) if not n30r else torch.zeros(1)
    n20r = flip("n20r", 1.0 / 2.0) if     n30r else torch.zeros(1)
    n31r = flip("n31r", 1.0 / 2.0) if not n20r else torch.zeros(1)
    n42r = flip("n42r", 1.0 / 2.0) if     n31r else torch.zeros(1)
    n43r = flip("n43r", 1.0 / 2.0) if not n31r else torch.zeros(1)
    n10r = flip("n10r", 1.0 / 2.0) if     n20r else torch.zeros(1)
    n21r = flip("n21r", 1.0 / 2.0) if not n10r else torch.zeros(1)
    n32r = flip("n32r", 1.0 / 2.0) if     n21r else torch.zeros(1)
    n33r = flip("n33r", 1.0 / 2.0) if not n21r else torch.zeros(1)
    n44r = flip("n44r", 1.0 / 2.0) if     n32r else torch.zeros(1)
    n45r = flip("n45r", 1.0 / 2.0) if not n32r else torch.zeros(1)
    n46r = flip("n46r", 1.0 / 2.0) if     n33r else torch.zeros(1)
    n47r = flip("n47r", 1.0 / 2.0) if not n33r else torch.zeros(1)
    n0   = n10r
    n10l = flip("n10l", 1.0 / 2.0) if     n0   else torch.zeros(1)
    n20l = flip("n20l", 1.0 / 2.0) if     n10l else torch.zeros(1)
    n21l = flip("n21l", 1.0 / 2.0) if not n10l else torch.zeros(1)
    n30l = flip("n30l", 1.0 / 2.0) if     n20l else torch.zeros(1)
    n31l = flip("n31l", 1.0 / 2.0) if not n20l else torch.zeros(1)
    n32l = flip("n32l", 1.0 / 2.0) if     n21l else torch.zeros(1)
    n33l = flip("n33l", 1.0 / 2.0) if not n21l else torch.zeros(1)
    n40l = flip("n40l", 1.0 / 2.0) if     n30l else torch.zeros(1)
    n41l = flip("n41l", 1.0 / 2.0) if not n30l else torch.zeros(1)
    n42l = flip("n42l", 1.0 / 2.0) if     n31l else torch.zeros(1)
    n43l = flip("n43l", 1.0 / 2.0) if not n31l else torch.zeros(1)
    n44l = flip("n44l", 1.0 / 2.0) if     n32l else torch.zeros(1)
    n45l = flip("n45l", 1.0 / 2.0) if not n32l else torch.zeros(1)
    n46l = flip("n46l", 1.0 / 2.0) if     n33l else torch.zeros(1)
    n47l = flip("n47l", 1.0 / 2.0) if not n33l else torch.zeros(1)
    return n47l

sites = ["npackets"]
truth = [0.3671875 * 3] # = 1.1015625

def model():
    npackets = pyro.sample("npackets", dist.Poisson(3))
    arrives = torch.tensor(0.0)
    if npackets.item() == 0.0:
        return arrives
    for ix in pyro.plate("packet", int(npackets.item())):
        arrives += network(suffix=f"_{ix}").item()
    return arrives

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    if args.num_runs > 1:
        print("not supported")
        import sys; sys.exit(1)

    else:
        # we are benchmarking, expect the same output as yodel
        torch.manual_seed(args.seed)
        np.random.seed(args.seed)
        random.seed(args.seed)
        start = time.time()
        importance = Importance(model, num_samples=args.num_samples)
        marginal = EmpiricalMarginal(importance.run())
        xs = marginal.mean.flatten()
        end = time.time()
        s = end - start
        print(" ".join([f"{x}" for x in xs]))
        print("{:.3f}ms".format(s * 1000))
        print(sum(compute_l1(xs, truth)))
