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

sites = ["npackets"]
truth = [2.3127377834371137]

model = lambda: mkarrival(3, probfn)

# ising_model = lambda: mkgrid(3, probfn)

# def model():
#     npackets = pyro.sample("npackets", dist.Poisson(3.0))
#     seen = 0.0
#     for _ in range(int(npackets.item())):
#         traverses = ising_model()[-1][-1] == 1.
#         seen += 1.0 if traverses else 0.0
#     return seen

# for x in range(3):
#   print(model())
# def guide():
#     lambda_ = pyro.param("lambda_", torch.tensor(4096.0), constraint=dist.constraints.positive)
#     npackets = pyro.sample("npackets", dist.Poisson(lambda_))
#     return network(npackets)

# # Importance sampling
#importance = Importance(model).run()
#marginal = EmpiricalMarginal(importance, sites=["npackets"])

# if __name__ == "__main__":
#     num_samples = 1_000
#     num_runs = 2
#     start_seed = 0
#     args = {}

    # l1s = []
    # times = []
    # for run, seedoffset in enumerate(range(1,num_runs+1)):
    #     seed = start_seed + seedoffset
    #     print(f"starting run {run+1}/{num_runs} (with seed: {seed})", flush=True)
    #     torch.manual_seed(seed)
    #     np.random.seed(seed)
    #     random.seed(seed)
    #     start = time.time()

    #     importance = Importance(model, num_samples=num_samples)
    #     posterior = importance.run(*args)

    #     ms = allmarg(posterior, sites, num_samples=num_samples) # don't think need a full
    #     print("am")
    #     end = time.time()
    #     times.append(end - start)
    #     print(times[-1], "s", flush=True)
    #     l1 = compute_l1(ms, truth)
    #     printit(sites, ms, l1)
    #     l1s.append(sum(l1))
    # #return (l1s, times)

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    if args.num_runs > 1:
        (l1s, times) = runall(model, sites, truth, num_runs=args.num_runs, num_samples=args.num_samples, start_seed=args.seed)
        print("--------")
        runs = len(l1s)
        print(f"averages over {runs} runs:")
        print("wallclock:", sum(times) / len(times), "s")
        print("       L1:", sum(l1s) / len(l1s))
    else:
        # we are benchmarking, expect the same output as yodel
        torch.manual_seed(args.seed)
        np.random.seed(args.seed)
        random.seed(args.seed)
        start = time.time()
        importance = Importance(model, num_samples=args.num_samples)
        posterior = importance.run()
        xs = [torch.tensor([tr.nodes["_RETURN"]["value"] for tr in importance.exec_traces]).mean()]
        end = time.time()
        s = end - start
        print(" ".join([f"{x}" for x in xs]))
        print("{:.3f}ms".format(s * 1000))
        # if s < 1.0:
        #     print("{:.3f}ms".format(s / 1000))
        # else:
        #     print("{:.3f}s".format(s))


# # Estimate the expectation
# samples = [marginal().item() for _ in range(num_samples)]

# print("Expected packets: ", sum(samples) / num_samples)
