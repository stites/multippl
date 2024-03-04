import pyro
import itertools
from abc import abstractmethod
import time

import torch
import torch.nn as nn
from torch.distributions import constraints

import pyro
import pyro.poutine as poutine
import pyro.distributions as dist
from pyro.distributions.torch import Categorical
from pyro.distributions.torch_distribution import TorchDistribution
from pyro.infer import SVI, Trace_ELBO, TraceEnum_ELBO, config_enumerate, infer_discrete
import pyro.distributions as dist
from pyro.infer import EmpiricalMarginal
from pyro.infer.importance import Importance

truth = torch.tensor([0.5, 1./4, 2./3, 2, 2, 3, 4, 5])
param_sites = ["pa", "pbt", "pbf", "pc", "pd_ab", "pd_anb", "pd_nab", "pd_nanb"]


#####################################################################################################
#####################################################################################################
#####################################################################################################
def params_for(names):
    return [pyro.param(name, torch.randn(1)) for name in names]

# @config_enumerate
# def model(data):
# # def model(pa, pbt, pbf, pc, pd_ab, pd_anb, pd_nab, pd_nanb, data):
#     #pa, pbt, pbf, pc, pd_ab, pd_anb, pd_nab, pd_nanb = params_for("pa", "pbt", "pbf", "pc", "pd_ab", "pd_anb", "pd_nab", "pd_nanb")
#     p = pyro.param("p", torch.randn(2, 2).exp(), constraint=constraints.simplex)
#     a = pyro.sample("x", dist.Categorical(p[0]))
#     b = pyro.sample("y", dist.Categorical(p[a]))
#     with pyro.plate("data", len(data)):
#         pyro.sample("obs", dist.Bernoulli(p), obs=data)

def get_item(ten, i):
    return ten[:, i].item() if len(ten.shape) > 0 else ten.item()

def istrue(ten, i):
    item = get_item(ten, i)
    return item if isinstance(item, bool) else item == 1.0

@config_enumerate(default="parallel")
def model(N):
    beta_param = pyro.param('beta_param', torch.randn(6).exp())
    c_param = pyro.param('c_param', torch.randn(1).exp())
    d_param = pyro.param('d_param', torch.randn(4).exp())

    # parameter
    pa = pyro.sample(param_sites[0], dist.Beta(beta_param[0], beta_param[1]))
    pbt = pyro.sample(param_sites[1], dist.Beta(beta_param[2],beta_param[3]))
    pbf = pyro.sample(param_sites[2], dist.Beta(beta_param[4], beta_param[5]))

    pc = pyro.sample(param_sites[3], dist.Normal(c_param[0], 1.0))

    pd_ab = pyro.sample(param_sites[4], dist.Normal(d_param[0], 1.5))
    pd_anb = pyro.sample(param_sites[5], dist.Normal(d_param[1], 1.5))
    pd_nab = pyro.sample(param_sites[6], dist.Normal(d_param[2], 1.5))
    pd_nanb = pyro.sample(param_sites[7], dist.Normal(d_param[3], 1.5))

    #out = torch.empty((N,4), dtype=torch.float64)
    for i in pyro.plate("data", N):
        a = pyro.sample(f"a_{i}", dist.Bernoulli(pa))
        match istrue(a, i):
            case True:
                b = pyro.sample(f"b_{i}", dist.Bernoulli(pbt.item()))
            case False:
                b = pyro.sample(f"b_{i}", dist.Bernoulli(pbf.item()))
        c = pyro.sample(f"c_{i}", dist.Normal(pc, 1.0))
        d = None
        match (istrue(a, i), istrue(b, i)):
            case (True, True):
                d = pyro.sample(f"d_{i}", dist.Normal(get_item(c, i) + pd_ab, 1.0))
            case (True, False):
                d = pyro.sample(f"d_{i}", dist.Normal(get_item(c, i) + pd_anb, 1.0))
            case (False, True):
                d = pyro.sample(f"d_{i}", dist.Normal(get_item(c, i) + pd_nab, 1.0))
            case (False, False):
                d = pyro.sample(f"d_{i}", dist.Normal(get_item(c, i) + pd_nanb, 1.0))
            case _:
                raise Exception("incomplete guard")
        #out[i] = torch.vstack([a, b, c, d]).T
    return torch.vstack([pa, pbt, pbf, pc, pd_ab, pd_anb, pd_nab, pd_nanb]).T


if __name__ == "__main__":

    import argparse
    parser = argparse.ArgumentParser("hybrid0")
    parser.add_argument("--num-samples", help="how many samples to run", default=1000, type=int)
    parser.add_argument("--seed", help="set seed", default=0, type=int)
    args = parser.parse_args()

    pyro.set_rng_seed(args.seed)
    num_samples = args.num_samples

    from generate import a, b, c, d, count
    a = torch.tensor(a).double()
    b = torch.tensor(b).double()
    c = torch.tensor(c).double()
    d = torch.tensor(d).double()
    N = count

    data = dict()
    for i in range(N):
      data[f"a_{i}"] = a[i]
      data[f"b_{i}"] = b[i]
      data[f"c_{i}"] = c[i]
      data[f"d_{i}"] = d[i]

    conditioned_model = pyro.condition(model, data=data)
    conditioned_model(N) # just a smoke test

    importance = Importance(conditioned_model, guide=None, num_samples=num_samples)

    print("running importance sampling...")
    start = time.time()
    emp_marginal = EmpiricalMarginal(importance.run(N), sites=param_sites)
    end = time.time()

    posterior_marg = emp_marginal.mean.detach()
    lws = torch.vstack(importance.log_weights)

    torch.set_printoptions(sci_mode=False, precision=3, linewidth=120)
    print("  estimates:", posterior_marg)
    print("      truth:", truth)
    print("         L1: {:.3f}".format((posterior_marg - truth).abs().sum().item()))
    print("        ESS: {:.3f} / {}".format(importance.get_ESS().item(), num_samples))
    print("Min/Max lw: {:.3f} <= log(w) <= {:.3f}".format(lws.min().item(), lws.max().item()))
    print("  E[log(w)]: {:.3f}".format(lws.mean().item()))
    print("Var[log(w)]: {:.3f}".format(lws.std().item()))
    print("-----------------------------------------")
    print("  wallclock: {:.3f}s".format(end - start))
    print()
