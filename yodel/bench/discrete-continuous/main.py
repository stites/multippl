import pyro
import itertools
from abc import abstractmethod
import time
import sys

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
import generate

param_sites = list(generate.truth.keys()) # ["et", "ef", "ft", "ff"] #
truth = torch.tensor([v for k, v in generate.truth.items() if k in param_sites])

#####################################################################################################
#####################################################################################################
#####################################################################################################
def params_for(names):
    return [pyro.param(name, torch.randn(1)) for name in names]

def get_item(ten, i):
    return ten[:, i].item() if len(ten.shape) > 0 else ten.item()

def istrue(ten, i):
    item = get_item(ten, i)
    return item if isinstance(item, bool) else item == 1.0

@config_enumerate
def model(N):
    pa = pyro.sample('a', dist.Beta(1.0, 1.0))
    pbt = pyro.sample('bt', dist.Beta(1.0, 1.0))
    pbf = pyro.sample('bf', dist.Beta(1.0, 1.0))
    # pct = pyro.sample('ct', dist.Beta(1.0, 1.0))
    # pcf = pyro.sample('cf', dist.Beta(1.0, 1.0))
    pct = pyro.param('ct', torch.tensor(7.0 / 8.0))
    pcf = pyro.param('cf', torch.tensor(1.0 / 8.0))
    pdtt = pyro.sample('dtt', dist.Beta(1.0, 1.0))
    pdtf = pyro.sample('dtf', dist.Beta(1.0, 1.0))
    pdft = pyro.sample('dft', dist.Beta(1.0, 1.0))
    pdff = pyro.sample('dff', dist.Beta(1.0, 1.0))
    pe_t = pyro.sample("et", dist.Normal(0.0, 1.0))
    pe_f = pyro.sample("ef", dist.Normal(2.0, 1.0))
    pf_t = pyro.sample("ft", dist.Normal(2.0, 1.0))
    pf_f = pyro.sample("ff", dist.Normal(0.0, 1.0))
    for i in pyro.plate("data", N):
        a = istrue(pyro.sample(f"a_{i}", dist.Bernoulli(pa)), i)
        b = istrue(pyro.sample(f"b_{i}", dist.Bernoulli(pbt if a else pbf)), i)
        c = istrue(pyro.sample(f"c_{i}", dist.Bernoulli(pct if a else pcf)), i)
        d = istrue(pyro.sample(f"d_{i}", dist.Bernoulli(pdtt if     b and     c else
                                                       (pdtf if     b and not c else
                                                       (pdft if not b and     c else
                                                        pdff)))), i)
        e = pyro.sample(f"e_{i}", dist.Normal(pe_t if b or d else pe_f, 1.0))
        f = pyro.sample(f"f_{i}", dist.Normal(pf_t if b or d else pf_f, 1.0))
        g = pyro.sample(f"g_{i}", dist.Normal(e + f, 1.0))

    return torch.vstack([pe_t, pe_f, pf_t, pf_f]).T



if __name__ == "__main__":

    import argparse
    parser = argparse.ArgumentParser("hybrid0")
    #parser.add_argument("--num-samples", help="how many samples to run", default=10, type=int)
    parser.add_argument("--num-samples", help="how many samples to run", default=1000, type=int)
    parser.add_argument("--seed", help="set seed", default=0, type=int)
    args = parser.parse_args()

    pyro.set_rng_seed(args.seed)
    num_samples = args.num_samples

    from generate import a, b, c, d, e, f, g, count
    a = torch.tensor(a).double()
    b = torch.tensor(b).double()
    c = torch.tensor(c).double()
    d = torch.tensor(d).double()
    e = torch.tensor(e).double()
    f = torch.tensor(f).double()
    g = torch.tensor(g).double()
    N = count


    data = dict()
    for i in range(N):
        #data[f"a_{i}"] = a[i]
        data[f"b_{i}"] = b[i]
        #data[f"c_{i}"] = c[i]
        data[f"d_{i}"] = d[i]
        #data[f"e_{i}"] = e[i]
        data[f"f_{i}"] = f[i]
        data[f"g_{i}"] = g[i]


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
    print(f" truth ({truth.shape[0]}):", truth)
    print("         L1: {:.3f}".format((posterior_marg - truth).abs().sum().item()))
    print("        ESS: {:.3f} / {}".format(importance.get_ESS().item(), num_samples))
    print("Min/Max lw: {:.3f} <= log(w) <= {:.3f}".format(lws.min().item(), lws.max().item()))
    print("  E[log(w)]: {:.3f}".format(lws.mean().item()))
    print("Var[log(w)]: {:.3f}".format(lws.std().item()))
    print("-----------------------------------------")
    print("  wallclock: {:.3f}s".format(end - start))
    print()
