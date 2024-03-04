'''
data generation for GMM-discrete-dynamic

The hybrid GMM-discrete-dynamic data generating process is:

p = 0.3
for n 0..3:
  hi ~ Bernoulli(p)
  g1 ~ Normal(if hi then +1.0 else -1.0, 1.0)
  g2 ~ Normal(if hi then +3.0 else -3.0, 1.0)
  ob = g1 + g2

  if ob > 0:
     x00 ~ flip 0.3
     x01 ~ if x00 then flip 0.4 else flip 0.6
     x10 ~ if x00 then flip 0.6 else flip 0.5
     x11 ~ if  x01 &&  x10 then flip 0.6 else
           if !x01 &&  x10 then flip 0.7 else
           if  x01 && !x10 then flip 0.8 else
           if !x01 && !x10 then flip 0.9 else
     p = if x11 then beta(0.3, 1.0) else beta beta(1.8, 1.0)
  else:
     x00 ~ flip 0.2
     x01 ~ if x00 then flip 0.3 else flip 0.5
     x10 ~ if x00 then flip 0.4 else flip 0.6
     x11 ~ if  x01 &&  x10 then flip 0.1 else
           if !x01 &&  x10 then flip 0.2 else
           if  x01 && !x10 then flip 0.3 else
           if !x01 && !x10 then flip 0.4 else
     p = if x11 then beta(0.8, 1.0) else beta beta(1.3, 1.0)

'''
import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from util import as_json

import pyro
import pyro.distributions as dist
import torch
import itertools
from collections import OrderedDict

pyro.set_rng_seed(3)

count = 5
#count = 100

dyn_truth = OrderedDict([("p", 0.3), ("pt_hi", 0.3), ("pf_hi", 1.8), ("pt_lo", 0.8), ("pf_lo", 1.3)])

gmm_truth = OrderedDict([ ("left_hi",1.0), ("right_hi",3.0), ("left_lo",-1.0), ("right_lo",-3.0)])
hi_truth = OrderedDict([("x00",0.3), ("x01t",0.4), ("x01f",0.6), ("x10t",0.6), ("x10f",0.5), ("x11tt",0.6), ("x11ft",0.7), ("x11tf",0.8), ("x11ff",0.9)])
lo_truth = OrderedDict([("x00",0.2), ("x01t",0.3), ("x01f",0.4), ("x10t",0.5), ("x10f",0.6), ("x11tt",0.1), ("x11ft",0.2), ("x11tf",0.3), ("x11ff",0.4)])
relabel = lambda post: (lambda kv: (kv[0]+post, kv[1]))
truth = OrderedDict(list(itertools.chain(dyn_truth.items(), gmm_truth.items(), map(relabel("_hi"), hi_truth.items()), map(relabel("_lo"), lo_truth.items()))))

def one_step(i, p, rix=None):
    hi = pyro.sample(f"hi{rix}_{i}", dist.Bernoulli(p))
    g1 = pyro.sample(f"g1{rix}_{i}", dist.Normal(torch.ones(1) * (gmm_truth['left_hi']  if hi else gmm_truth['left_lo']), scale=torch.ones(1)))
    g2 = pyro.sample(f"g2{rix}_{i}", dist.Normal(torch.ones(1) * (gmm_truth['right_hi'] if hi else gmm_truth['right_lo']), scale=torch.ones(1)))
    g = g1 + g2
    go_hi = (g > 0)
    if go_hi:
        x00 = pyro.sample(f"x00{rix}_{i}", dist.Bernoulli(hi_truth["x00"]))
        x01 = pyro.sample(f"x01{rix}_{i}", dist.Bernoulli(hi_truth["x01t"] if x00 else hi_truth["x01f"]))
        x10 = pyro.sample(f"x10{rix}_{i}", dist.Bernoulli(hi_truth["x10t"] if x00 else hi_truth["x10f"]))
        x11 = pyro.sample(f"x11{rix}_{i}", dist.Bernoulli(hi_truth["x11tt"] if     x01 and     x10 else
                                                         (hi_truth["x11ft"] if not x01 and     x10 else
                                                         (hi_truth["x11tf"] if     x01 and not x10 else
                                                          hi_truth["x11ff"]))))

        pnxt = pyro.sample(f"p{rix}_{i}", dist.Beta(dyn_truth["pt_hi"], 1.0) if x11 else dist.Beta(dyn_truth["pf_hi"], 1.0))
    else:
        x00 = pyro.sample(f"x00{rix}_{i}", dist.Bernoulli(lo_truth["x00"]))
        x01 = pyro.sample(f"x01{rix}_{i}", dist.Bernoulli(lo_truth["x01t"] if x00 else lo_truth["x01f"]))
        x10 = pyro.sample(f"x10{rix}_{i}", dist.Bernoulli(lo_truth["x10t"] if x00 else lo_truth["x10f"]))
        x11 = pyro.sample(f"x11{rix}_{i}", dist.Bernoulli(lo_truth["x11tt"] if     x01 and     x10 else
                                                         (lo_truth["x11ft"] if not x01 and     x10 else
                                                         (lo_truth["x11tf"] if     x01 and not x10 else
                                                          lo_truth["x11ff"]))))
        pnxt = pyro.sample(f"p{rix}_{i}", dist.Beta(dyn_truth["pt_lo"], 1.0) if x11 else dist.Beta(dyn_truth["pf_lo"], 1.0))
    return [hi, g1, g2, g, x00, x01, x10, x11, pnxt]


def multi_step():
    hss  = []
    g1ss = []
    g2ss = []
    gss  = []
    x00s = []
    x10s = []
    x01s = []
    x11s = []
    for i in pyro.plate("data", count):
        hs = []
        g1s = []
        g2s = []
        gs = []
        xs = [[[], []], [[], []]]
        round_ix = 0
        for round_ix in [0, 1, 2]:
            [hi, g1, g2, g, x00, x01, x10, x11, pnxt] = one_step(i, dyn_truth["p"], rix=str(round_ix))
            hs.append(hi)
            g1s.append(g1)
            g2s.append(g2)
            gs.append(g)
            xs[0][0].append(x00)
            xs[1][0].append(x10)
            xs[0][1].append(x01)
            xs[1][1].append(x11)
        hss.append(torch.vstack(hs).bool())
        g1ss.append(torch.vstack(g1s))
        g2ss.append(torch.vstack(g2s))
        gss.append(torch.vstack(gs))
        x00s.append(torch.vstack(xs[0][0]).bool())
        x10s.append(torch.vstack(xs[1][0]).bool())
        x01s.append(torch.vstack(xs[0][1]).bool())
        x11s.append(torch.vstack(xs[1][1]).bool())

    return [hss, g1ss, g2ss, gss, x00s, x10s, x01s, x11s]

state = multi_step()

def invert(xss):
    o = torch.stack(xss)
    return [o[:,rix] for rix in [0, 1, 2] ]

[hss, g1ss, g2ss, gss, x00s, x10s, x01s, x11s] = list(map(invert, state))


if __name__ == "__main__":
    serialize = list(itertools.chain(hss, g1ss, g2ss, gss, x00s, x10s, x01s, x11s))

    def roundix(k):
        return [f"{k}_{rix}_" for rix in [0, 1, 2]]

    col_names = list(itertools.chain.from_iterable([roundix(c) for c in ["hs", "g1s", "g2s", "gs", "x00", "x10", "x01", "x11"]]))

    keys = as_json(*serialize, batch_size=count, outname="data", outext="json", col_names=col_names)
    print(sorted(list(keys)))
