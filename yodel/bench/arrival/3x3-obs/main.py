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
       case (0, _): return [1./3, 1./4]
       case (_, 0): return [1./5, 1./6]
       case (1, 1): return [1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0]
       case (2, 1): return [2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0]
       case (1, 2): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
       case (2, 2): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case _: raise Exception

truth = [0.291462009 * 3.0] # x01 | !x21 = 0.874386027

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


model = lambda: mkarrival01(3, probfn, conditions=[(lambda ix: f"flip21_{ix}", torch.tensor(0.0))])

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    if args.num_runs > 1:
        print("not supported")
        sys.exit(1)

    else:
        torch.manual_seed(args.seed)
        np.random.seed(args.seed)
        random.seed(args.seed)
        start = time.time()
        importance = Importance(model, num_samples=args.num_samples)
        posterior = importance.run()
        marginal = EmpiricalMarginal(posterior)
        xs = marginal.mean.flatten()
        end = time.time()
        s = end - start
        print(" ".join([f"{x}" for x in xs]))
        print("{:.3f}ms".format(s * 1000))
        #print(sum(compute_l1(xs, truth)))
