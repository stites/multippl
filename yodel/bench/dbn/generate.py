'''
'''
# inspired by pyro:
#
# By adding a dependency of x on w, we generalize to a
# Dynamic Bayesian Network.
#
#     w[t-1] ----> w[t] ---> w[t+1]
#        |  \       |  \       |   \
#        | x[t-1] ----> x[t] ----> x[t+1]
#        |   /      |   /      |   /
#        V  /       V  /       V  /
#     y[t-1]       y[t]      y[t+1]
#

# data generation file for hybridCD

# The hybridCD network looks like the following:

# class ~ Categorical([0.3, 0.7])        # 0 or 1
# sample ~ Normal((class * 2) - 1, 1.0)  # N(±1, 1)     # observable
# if sample > 0:
#    x ~ discrete([0.2, 0.4, 0.5])
#    if x == 1 then flip 0.5 else
#    if x == 2 then flip 0.2 else
#    if x == 3 then flip 0.3 else ⊥
# else:
#    x ~ flip 0.8
#    if x then flip 0.5 else flip 0.2
# '''
# import os, sys
# sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# from util import as_json

# import random
# import numpy as np
# from numpy.random import normal
# import pyro
# import pyro.distributions as dist
# import torch

# pyro.set_rng_seed(3)

# count = 10

# def flips(p, l):
#     return np.array([random.random() < p for _ in range(l)])

# def conditional_flips(ps, cond_on):
#     (pt, pf) = ps
#     t_ix = np.where(cond_on == True)
#     t = flips(pt, t_ix[0].shape[0])
#     f_ix = np.where(cond_on == False)
#     f = flips(pf, f_ix[0].shape[0])
#     nxt = np.empty((cond_on.shape[0],), dtype=bool)
#     nxt[t_ix] = t
#     nxt[f_ix] = f
#     return torch.from_numpy(nxt), t, f

# cls = []
# ds = []
# xs = []
# ys = []
# for i in pyro.plate("data", count):
#     c = pyro.sample(f"cls_{i}", dist.Categorical(torch.tensor([0.3, 0.7])))
#     d = pyro.sample(f"dist_{i}", dist.Normal((c.double() * 2) - 1, torch.ones(1)))
#     x, y = (None, None)
#     if d > 0.0:
#         x = pyro.sample(f"x_{i}", dist.Categorical(torch.tensor([0.2, 0.3, 0.5])))
#         match x.item():
#             case 0:
#                 y = pyro.sample(f"y_{i}", dist.Bernoulli(0.5))
#             case 1:
#                 y = pyro.sample(f"y_{i}", dist.Bernoulli(0.2))
#             case 2:
#                 y = pyro.sample(f"y_{i}", dist.Bernoulli(0.3))
#             case x:
#                 raise Exception(f"unaccounted for case: {x}")
#     else:
#         x = pyro.sample(f"x_{i}", dist.Bernoulli(0.8))
#         y = pyro.sample(f"y_{i}", dist.Bernoulli(0.5)) if x.item() else pyro.sample(f"y_{i}", dist.Bernoulli(0.2))
#     cls.append(c)
#     ds.append(d)
#     xs.append(x)
#     ys.append(y)

# cls = torch.vstack(cls)
# ds = torch.vstack(ds)
# x = torch.vstack(xs)
# y = torch.vstack(ys)

# if __name__ == "__main__":
#     keys = as_json(cls.double() + 1, ds, x.bool(), y.bool(), batch_size=5, outname="data", outext="json", col_names=["class", "dist", "xs", "ys"])
#     print(sorted(list(keys)))
