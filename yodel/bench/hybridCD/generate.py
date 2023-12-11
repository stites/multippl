'''
data generation file for hybridCD

The hybridCD network looks like the following:

c    clss = [cls0]
    dss = [ds0]
    (x00s, x10s, x01s, x11s) = ([x00], [x10], [x01], [x11])

lass ~ Categorical(0.3, 0.7)        # 0 or 1
sample ~ Normal((class * 2) - 1, 1.0)  # N(±1, 1)     # observable
if sample > 0:
   x00 ~ flip 0.3
   x01 ~ if x00 then flip 0.4 else flip 0.6
   x10 ~ if x00 then flip 0.6 else flip 0.5
   x11 = x01 && x10
else:
   x00 ~ flip 0.2
   x01 ~ if x00 then flip 0.3 else flip 0.5
   x10 ~ if x00 then flip 0.4 else flip 0.6
   x11 = x01 && x10
'''
import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from util import as_json

import pyro
import pyro.distributions as dist
import torch

pyro.set_rng_seed(3)

count = 100

pop_truth = 0.0

outlier_truth = dict(x00=0.3, x01t=0.4, x01f=0.6, x10t=0.6, x10f=0.5)
normals_truth = dict(x00=0.2, x01t=0.3, x01f=0.4, x10t=0.5, x10f=0.6)
d2a = lambda d: [d['x00'], d['x01t'], d['x01f'], d['x10t'], d['x10f']]
truth = [pop_truth] + d2a(outlier_truth) + d2a(normals_truth)

#def one_step(prev_cls=None, prev_x11=None, rix=None):
def one_step(rix=None):
    cls = []
    ps = []
    xs = [[[], []], [[], []]]
    rix = "" if rix is None else f"_{rix}"
    for i in pyro.plate("data", count):
        # if prev_cls is None:
        #     c = pyro.sample(f"cls{rix}_{i}", dist.Categorical(torch.tepsor([0.3, 0.7])))
        # else:
        #     pc = prev_cls[i]
        #     c = pc if prev_x11[i] else pc + 1 % 2
        # d = pyro.sample(f"pop{rix}_{i}", dist.Normal((c.double() * 2) - 1, torch.ones(1)))
        p = pyro.sample(f"pop{rix}_{i}", dist.Normal(torch.ones(1) * pop_truth, torch.ones(1)))
        if (p > -1.0 or p < 1.0): # we are in the average case
            x00 = pyro.sample(f"x00{rix}_{i}", dist.Bernoulli(outlier_truth["x00"]))
            x01 = pyro.sample(f"x01{rix}_{i}", dist.Bernoulli(outlier_truth["x01t"] if x00 else outlier_truth["x01f"]))
            x10 = pyro.sample(f"x10{rix}_{i}", dist.Bernoulli(outlier_truth["x10t"] if x00 else outlier_truth["x10f"]))
            x11 = x01 or x10
        else:
            x00 = pyro.sample(f"x00{rix}_{i}", dist.Bernoulli(normals_truth["x00"]))
            x01 = pyro.sample(f"x01{rix}_{i}", dist.Bernoulli(normals_truth["x01t"] if x00 else normals_truth["x01f"]))
            x10 = pyro.sample(f"x10{rix}_{i}", dist.Bernoulli(normals_truth["x10t"] if x00 else normals_truth["x10f"]))
            x11 = x01 or x10
        #cls.append(c)
        ps.append(p)
        xs[0][0].append(x00)
        xs[1][0].append(x10)
        xs[0][1].append(x01)
        xs[1][1].append(x11)

    #cls = torch.vstack(cls)
    ps = torch.vstack(ps)
    x00 = torch.vstack(xs[0][0])
    x10 = torch.vstack(xs[1][0])
    x01 = torch.vstack(xs[0][1])
    x11 = torch.vstack(xs[1][1])

    #return [cls, ps, x00, x10, x01, x11]
    return [ps, x00, x10, x01, x11]


# def psteps(i:int):
#     (cls0, ds0, x00, x10, x01, x11) = one_step()

#     (clss, dss) = ([cls0], [ds0])
#     (x00s, x10s, x01s, x11s) = ([x00], [x10], [x01], [x11])

#     for rix in range(max(0, i-1)):
#         (cls, ds, x00, x10, x01, x11) = one_step(cls0, x11, rix + 1)
#         clss.append(cls)
#         dss.append(ds)
#         x00s.append(x00)
#         x10s.append(x10)
#         x01s.append(x01)
#         x11s.append(x11)
#     return (clss, dss, x00s, x10s, x10s, x11s)

(ps, x00, x10, x01, x11) = one_step()

if __name__ == "__main__":
    #(cls, ps, x00, x10, x01, x11) = one_step()
    keys = as_json(ps, x00.bool(), x10.bool(), x01.bool(), x11.bool(), batch_size=5, outname="data", outext="json", col_names=["pop", "o00_", "o10_", "o01_", "o11_"])
    print(sorted(list(keys)))
