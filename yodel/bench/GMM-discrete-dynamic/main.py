import torch
import time
import pyro
import pyro.distributions as dist
from pyro.infer import config_enumerate
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
import sys
import generate


truth = torch.tensor([v for k, v in generate.truth.items() if k[0] == 'x'])
sites = [k for k in generate.truth.keys() if k[0] == 'x']

def root(n, i, p, suffix="", obs=None):
    return pyro.sample(f"{n}{suffix}_{i}", dist.Bernoulli(p), obs=obs)

def node1(n, i, parent, t, f, suffix="", obs=None):
    return pyro.sample(f"{n}{suffix}_{i}", dist.Bernoulli(t if parent else f), obs=obs)

def node2(n, i, p1, p2, tt, tf, ft, ff, suffix="", obs=None):
    return pyro.sample(f"{n}{suffix}_{i}",
                       dist.Bernoulli(tt if     p1 and     p2 else
                                     (ft if not p1 and     p2 else
                                     (tf if     p1 and not p2 else
                                      ff))), obs=obs)

@config_enumerate
def network(i, p00, p10t, p10f, p01t, p01f, p11tt, p11tf, p11ft, p11ff, o00, o10, o01, o11, suffix=""):
    f00 = root("x00", i, p00, suffix=suffix)
    f10 = node1("x10", i, f00, p10t, p10f, suffix=suffix)
    f01 = node1("x01", i, f00, p01t, p01f, suffix=suffix)
    f11 = node2("x11", i, f01, f10, p11tt, p11tf, p11ft, p11ff, suffix=suffix, obs=o11)

@config_enumerate
def mixture(i, p00, p10t, p10f, p01t, p01f, p11tt, p11tf, p11ft, p11ff, g, o00, o10, o01, o11):
  if (g > 0):
      return network(i, p00[0], p10t[0], p10f[0], p01t[0], p01f[0], p11tt[0], p11tf[0], p11ft[0], p11ff[0], o00, o01, o10, o11, suffix="_hi")
  else:
      return network(i, p00[1], p10t[1], p10f[1], p01t[1], p01f[1], p11tt[1], p11tf[1], p11ft[1], p11ff[1], o00, o01, o10, o11, suffix="_lo")

@config_enumerate
def model(N, g, o00, o10, o01, o11):
    p00_hi  = pyro.sample("x00_hi", dist.Beta(1.0, 1.0))
    p10t_hi = pyro.sample("x10t_hi", dist.Beta(1.0, 1.0))
    p10f_hi = pyro.sample("x10f_hi", dist.Beta(1.0, 1.0))
    p01t_hi = pyro.sample("x01t_hi", dist.Beta(1.0, 1.0))
    p01f_hi = pyro.sample("x01f_hi", dist.Beta(1.0, 1.0))
    p11tt_hi = pyro.sample("x11tt_hi", dist.Beta(1.0, 1.0))
    p11tf_hi = pyro.sample("x11tf_hi", dist.Beta(1.0, 1.0))
    p11ft_hi = pyro.sample("x11ft_hi", dist.Beta(1.0, 1.0))
    p11ff_hi = pyro.sample("x11ff_hi", dist.Beta(1.0, 1.0))

    p00_lo  = pyro.sample("x00_lo", dist.Beta(1.0, 1.0))
    p10t_lo = pyro.sample("x10t_lo", dist.Beta(1.0, 1.0))
    p10f_lo = pyro.sample("x10f_lo", dist.Beta(1.0, 1.0))
    p01t_lo = pyro.sample("x01t_lo", dist.Beta(1.0, 1.0))
    p01f_lo = pyro.sample("x01f_lo", dist.Beta(1.0, 1.0))
    p11tt_lo = pyro.sample("x11tt_lo", dist.Beta(1.0, 1.0))
    p11tf_lo = pyro.sample("x11tf_lo", dist.Beta(1.0, 1.0))
    p11ft_lo = pyro.sample("x11ft_lo", dist.Beta(1.0, 1.0))
    p11ff_lo = pyro.sample("x11ff_lo", dist.Beta(1.0, 1.0))

    p00 = (p00_hi, p00_lo)
    p10t = (p10t_hi, p10t_lo)
    p10f = (p10f_hi, p10f_lo)
    p01t = (p01t_hi, p01t_lo)
    p01f = (p01f_hi, p01f_lo)
    p11tt = (p11tt_hi, p11tt_lo)
    p11tf = (p11tf_hi, p11tf_lo)
    p11ft = (p11ft_hi, p11ft_lo)
    p11ff = (p11ff_hi, p11ff_lo)

    for i in pyro.plate("data", N):
        mixture(i, p00, p10t, p10f, p01t, p01f, p11tt, p11tf, p11ft, p11ff, g[i], o00[i], o10[i], o01[i], o11[i]);

    return [p00_hi , p10t_hi, p10f_hi, p01t_hi, p01f_hi, p11tt_hi, p11tf_hi, p11ft_hi, p11ff_hi, p00_lo , p10t_lo, p10f_lo, p01t_lo, p01f_lo, p11tt_lo, p11tf_lo, p11ft_lo, p11ff_lo]

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=10, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    from generate import hs, g1s, g2s, gs, x00, x10, x01, x11
    # rng is set in generate, need to re-set it after import
    pyro.set_rng_seed(args.seed)

    # smoke test
    # model(2, torch.tensor([0.8032760620117188, 0.17483338713645935]), torch.zeros(2), torch.zeros(3), torch.ones(4), torch.ones(5))

    model_args = [len(gs), gs, x00.double(), x10.double(), x01.double(), x11.double(),]


    print("running importance sampling...")
    start = time.time()
    importance = Importance(model, guide=None, num_samples=args.num_samples)
    emp_marginal = EmpiricalMarginal(importance.run(*model_args), sites=sites)
    end = time.time()

    posterior_marg = emp_marginal.mean.detach()
    lws = torch.vstack(importance.log_weights)

    torch.set_printoptions(sci_mode=False, precision=3, linewidth=120)
    print("  estimates:", posterior_marg)
    print(f" truth ({truth.shape[0]}):", truth)
    print("         L1: {:.3f}".format((posterior_marg - truth).abs().sum().item()))
    #print("        ESS: {:.3f} / {}".format(importance.get_ESS().item(), num_samples))
    print("Min/Max lw: {:.3f} <= log(w) <= {:.3f}".format(lws.min().item(), lws.max().item()))
    print("  E[log(w)]: {:.3f}".format(lws.mean().item()))
    print("Var[log(w)]: {:.3f}".format(lws.std().item()))
    print("-----------------------------------------")
    print("  wallclock: {:.3f}s".format(end - start))
    print()

    # (l1s, times) = runall(model, sites, truth, args.num_runs, args.num_samples, *model_args, start_seed=args.seed)

    # print("--------")
    # runs = len(l1s)
    # print(f"averages over {runs} runs:")
    # print("wallclock:", sum(times) / len(times), "s")
    # print("       L1:", sum(l1s) / len(l1s))
