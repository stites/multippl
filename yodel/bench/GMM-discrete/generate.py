'''
data generation for GMM-discrete

The hybrid GMM-discrete network is defined as:

hi ~ Bernoulli(0.3)
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
else:
   x00 ~ flip 0.2
   x01 ~ if x00 then flip 0.3 else flip 0.5
   x10 ~ if x00 then flip 0.4 else flip 0.6
   x11 ~ if  x01 &&  x10 then flip 0.1 else
         if !x01 &&  x10 then flip 0.2 else
         if  x01 && !x10 then flip 0.3 else
         if !x01 && !x10 then flip 0.4 else
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

gmm_truth = OrderedDict([("hi",0.3), ("left_hi",1.0), ("right_hi",3.0), ("left_lo",-1.0), ("right_lo",-3.0)])

hi_truth = OrderedDict([("x00",0.3), ("x01t",0.4), ("x01f",0.6), ("x10t",0.6), ("x10f",0.5), ("x11tt",0.6), ("x11ft",0.7), ("x11tf",0.8), ("x11ff",0.9)])
lo_truth = OrderedDict([("x00",0.2), ("x01t",0.3), ("x01f",0.4), ("x10t",0.5), ("x10f",0.6), ("x11tt",0.1), ("x11ft",0.2), ("x11tf",0.3), ("x11ff",0.4)])
relabel = lambda post: (lambda kv: (kv[0]+post, kv[1]))
truth = OrderedDict(list(itertools.chain(gmm_truth.items(), map(relabel("_hi"), hi_truth.items()), map(relabel("_lo"), lo_truth.items()))))

d2a = lambda d: [d['x00'], d['x01t'], d['x01f'], d['x10t'], d['x10f']]
# truth = [pop_truth] + [boundary] + d2a(hi_truth) + d2a(lo_truth)


# hi ~ Bernoulli(0.3)
# g1 ~ Normal(if hi then +1.0 else -1.0, 1.0)
# g2 ~ Normal(if hi then +3.0 else -3.0, 1.0)
# ob = g1 + g2
#
# if ob > 0:
#    x00 ~ flip 0.3
#    x01 ~ if x00 then flip 0.4 else flip 0.6
#    x10 ~ if x00 then flip 0.6 else flip 0.5
#    x11 ~ if  x01 &&  x10 then flip 0.6 else
#          if !x01 &&  x10 then flip 0.7 else
#          if  x01 && !x10 then flip 0.8 else
#          if !x01 && !x10 then flip 0.9 else
# else:
#    x00 ~ flip 0.2
#    x01 ~ if x00 then flip 0.3 else flip 0.5
#    x10 ~ if x00 then flip 0.4 else flip 0.6
#    x11 ~ if  x01 &&  x10 then flip 0.1 else
#          if !x01 &&  x10 then flip 0.2 else
#          if  x01 && !x10 then flip 0.3 else
#          if !x01 && !x10 then flip 0.4 else

#def one_step(prev_cls=None, prev_x11=None, rix=None):
def one_step(rix=None):
    hs = []
    g1s = []
    g2s = []
    gs = []
    xs = [[[], []], [[], []]]
    rix = "" if rix is None else f"_{rix}"
    for i in pyro.plate("data", count):
        hi = pyro.sample(f"hi{rix}_{i}", dist.Bernoulli(gmm_truth['hi']))
        g1 = pyro.sample(f"g1{rix}_{i}", dist.Normal(torch.ones(1) * (gmm_truth['left_hi']  if hi else gmm_truth['left_lo']), scale=torch.ones(1)))
        g2 = pyro.sample(f"g2{rix}_{i}", dist.Normal(torch.ones(1) * (gmm_truth['right_hi'] if hi else gmm_truth['right_lo']), scale=torch.ones(1)))
        g = g1 + g2
        if (g > 0):
            x00 = pyro.sample(f"x00{rix}_{i}", dist.Bernoulli(hi_truth["x00"]))
            x01 = pyro.sample(f"x01{rix}_{i}", dist.Bernoulli(hi_truth["x01t"] if x00 else hi_truth["x01f"]))
            x10 = pyro.sample(f"x10{rix}_{i}", dist.Bernoulli(hi_truth["x10t"] if x00 else hi_truth["x10f"]))
            x11 = pyro.sample(f"x11{rix}_{i}", dist.Bernoulli(hi_truth["x11tt"] if     x01 and     x10 else
                                                             (hi_truth["x11ft"] if not x01 and     x10 else
                                                             (hi_truth["x11tf"] if     x01 and not x10 else
                                                              hi_truth["x11ff"]))))
        else:
            x00 = pyro.sample(f"x00{rix}_{i}", dist.Bernoulli(lo_truth["x00"]))
            x01 = pyro.sample(f"x01{rix}_{i}", dist.Bernoulli(lo_truth["x01t"] if x00 else lo_truth["x01f"]))
            x10 = pyro.sample(f"x10{rix}_{i}", dist.Bernoulli(lo_truth["x10t"] if x00 else lo_truth["x10f"]))
            x11 = pyro.sample(f"x11{rix}_{i}", dist.Bernoulli(lo_truth["x11tt"] if     x01 and     x10 else
                                                             (lo_truth["x11ft"] if not x01 and     x10 else
                                                             (lo_truth["x11tf"] if     x01 and not x10 else
                                                              lo_truth["x11ff"]))))
        hs.append(hi)
        g1s.append(g1)
        g2s.append(g2)
        gs.append(g)
        xs[0][0].append(x00)
        xs[1][0].append(x10)
        xs[0][1].append(x01)
        xs[1][1].append(x11)

    hs = torch.vstack(hs).bool()
    g1s = torch.vstack(g1s)
    g2s = torch.vstack(g2s)
    gs = torch.vstack(gs)
    x00 = torch.vstack(xs[0][0]).bool()
    x10 = torch.vstack(xs[1][0]).bool()
    x01 = torch.vstack(xs[0][1]).bool()
    x11 = torch.vstack(xs[1][1]).bool()

    return [hs, g1s, g2s, gs, x00, x10, x01, x11]


state = one_step()
[hs, g1s, g2s, gs, x00, x10, x01, x11] = state

if __name__ == "__main__":
    #(cls, ps, x00, x10, x01, x11) = one_step()
    keys = as_json(*state, batch_size=5, outname="data", outext="json", col_names=["hs", "g1s", "g2s", "gs", "x00_", "x10_", "x01_", "x11_"])
    print(sorted(list(keys)))
