import torch
import time
import pyro
import pyro.distributions as dist
from pyro.infer import config_enumerate
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
import generate


truth = torch.tensor(generate.truth)
network_sites = lambda i: [f"p00{i}", f"p10T{i}", f"p10F{i}", f"p01T{i}", f"p01F{i}"]
sites = ["np", "b"] + network_sites(0) + network_sites(1)

def flip(n, p, suffix="", obs=None):
    return pyro.sample("flip"+n+suffix, dist.Bernoulli(p), obs=obs)

def edge(n, parent, t, f, obs=None):
    return flip(n, t, suffix="t", obs=obs) if parent == 1 else flip(n, f, suffix="f", obs=obs)

@config_enumerate
def network(i, p00, p10t, p10f, p01t, p01f, o00, o10, o01, o11):
    f00 = flip(f"00_{i}", p00, obs=o00)
    f10 = edge(f"10_{i}", f00, p10t, p10f, obs=o10)
    f01 = edge(f"01_{i}", f00, p01t, p01f, obs=o01)
    f11 = f01 or f10 # don't observe anything here as we "know" this is deterministic
    # x11 = node("11", x10, x01, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    return [[f00, f01],
            [f10, f11]]

@config_enumerate
def mixture(i, np, boundary, x00P , x10PT, x10PF, x01PT, x01PF, n, o00, o10, o01, o11):
  pyro.sample(f"n_{i}", dist.Normal(np, 1.0), obs=n)
  if (n > boundary or n < (-boundary)):
      return network(i, x00P[0], x10PT[0], x10PF[0], x01PT[0], x01PF[0], o00, o01, o10, o11)
  else:
      return network(i, x00P[1], x10PT[1], x10PF[1], x01PT[1], x01PF[1], o00, o01, o10, o11)

@config_enumerate
def model(N, n, o00, o10, o01, o11):
    b = pyro.sample("b", dist.Uniform(0.5, 1.5))
    np = pyro.sample("np", dist.Normal(0.0, 1.0))

    p000  = pyro.sample("p000", dist.Beta(1.0, 1.0))
    p10T0 = pyro.sample("p10T0", dist.Beta(1.0, 1.0))
    p10F0 = pyro.sample("p10F0", dist.Beta(1.0, 1.0))
    p01T0 = pyro.sample("p01T0", dist.Beta(1.0, 1.0))
    p01F0 = pyro.sample("p01F0", dist.Beta(1.0, 1.0))

    p001  = pyro.sample("p001", dist.Beta(1.0, 1.0))
    p10T1 = pyro.sample("p10T1", dist.Beta(1.0, 1.0))
    p10F1 = pyro.sample("p10F1", dist.Beta(1.0, 1.0))
    p01T1 = pyro.sample("p01T1", dist.Beta(1.0, 1.0))
    p01F1 = pyro.sample("p01F1", dist.Beta(1.0, 1.0))

    for i in pyro.plate("data", N):
        mixture(i, np, b, (p000, p001), (p10T0, p10T1), (p10F0, p10F1), (p01T0, p01T1), (p01F0, p01F1), n[i], o00[i], o10[i], o01[i], o11[i]);

    return [b, np, p000 , p10T0, p10F0, p01T0, p01F0, p001 , p10T1, p10F1, p01T1, p01F1]


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=10_000, type=int,)
    parser.add_argument("--num-runs", default=10, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    from generate import ps, x00, x10, x01, x11
    from utils import *

    # smoke test
    # model(2, torch.tensor([0.8032760620117188, 0.17483338713645935]), torch.zeros(2), torch.zeros(3), torch.ones(4), torch.ones(5))

    model_args = [len(ps), ps, x00, x10, x01, x11,]
    (l1s, times) = runall(model, sites, truth, args.num_runs, args.num_samples, *model_args, start_seed=args.seed)

    print("--------")
    runs = len(l1s)
    print(f"averages over {runs} runs:")
    print("wallclock:", sum(times) / len(times), "s")
    print("       L1:", sum(l1s) / len(l1s))

