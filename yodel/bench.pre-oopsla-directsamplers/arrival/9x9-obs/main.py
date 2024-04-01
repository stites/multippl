import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import torch
from pyro.infer import Importance
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


def mkarrival01(n, probfn, conditions=[]):
    npackets = pyro.sample("npackets", dist.Poisson(3))
    arrives = torch.zeros(1)
    if npackets.item() == 0.0:
        return arrives

    for ix in pyro.plate("packet", int(npackets.item())):
        m = pyro.condition(mkgrid, data={k(ix): v for k, v in conditions})
        o = m(n, probfn, suffix="_"+str(ix))
        arrives += o[0][1].item()
    return arrives


truth = [0.875000001]
model = lambda: mkarrival01(9, probfn, conditions=[(lambda ix: f"flip81_{ix}", torch.tensor(0.0))])

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    if args.num_runs > 1:
        print("not supported")
        sys.exit(1)

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
