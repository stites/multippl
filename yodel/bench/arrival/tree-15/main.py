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
    n30r = True

    n20r = flip("n20r", 1.0 / 3.0) if     n30r else torch.zeros(1)
    n31r = flip("n31r", 1.0 / 3.0) if not n20r else torch.zeros(1)

    n10r = flip("n10r", 1.0 / 3.0) if     n20r else torch.zeros(1)
    n21r = flip("n21r", 1.0 / 3.0) if not n10r else torch.zeros(1)
    n32r = flip("n32r", 1.0 / 3.0) if     n21r else torch.zeros(1)
    n33r = flip("n33r", 1.0 / 3.0) if not n21r else torch.zeros(1)
    n0   = n10r
    n10l = flip("n10l", 1.0 / 3.0) if     n0   else torch.zeros(1)

    n20l = flip("n20l", 1.0 / 3.0) if     n10l else torch.zeros(1)
    n21l = flip("n21l", 1.0 / 3.0) if not n10l else torch.zeros(1)

    n30l = flip("n30l", 1.0 / 3.0) if     n20l else torch.zeros(1)
    n31l = flip("n31l", 1.0 / 3.0) if not n20l else torch.zeros(1)
    n32l = flip("n32l", 1.0 / 3.0) if     n21l else torch.zeros(1)
    n33l = flip("n33l", 1.0 / 3.0) if not n21l else torch.zeros(1)

    return n0

truth = [0.076923077]

def model():
    npackets = pyro.sample("npackets", dist.Poisson(3))
    arrives = torch.zeros(1)
    if npackets.item() == 0.0:
        return arrives
    for ix in pyro.plate("packet", int(npackets.item())):
        m = pyro.condition(network, data={f"n32l_{ix}": torch.tensor(0.0)})
        arrives += m(suffix=f"_{ix}").item()
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
        #print(sum(compute_l1(xs, truth)))
