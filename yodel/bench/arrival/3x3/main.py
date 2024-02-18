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
if __name__ == "__main__":
    num_samples = 1_000
    num_runs = 2
    start_seed = 0
    args = {}
    sites = ["npackets"]
    truth = [2.3127377834371137]

    model = lambda: mkarrival(3, probfn)

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



# # Estimate the expectation
# samples = [marginal().item() for _ in range(num_samples)]

# print("Expected packets: ", sum(samples) / num_samples)
