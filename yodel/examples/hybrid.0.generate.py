import random
import json
import pandas as pd
import numpy as np
from numpy.random import normal

random.seed(3)
count = 10

def flips(p, l):
    return np.array([random.random() < p for _ in range(l)])

def conditional_flips(ps, cond_on):
    (pt, pf) = ps
    t_ix = np.where(cond_on == True)
    t = flips(pt, t_ix[0].shape[0])
    f_ix = np.where(cond_on == False)
    f = flips(pf, f_ix[0].shape[0])
    nxt = np.empty((cond_on.shape[0],), dtype=bool)
    nxt[t_ix] = t
    nxt[f_ix] = f
    return nxt

a = flips(0.5, count)
b = conditional_flips((1./4, 2./3), a)
c = normal(loc=2, scale=0.3, size=count)

def noise(scale, size=count):
    return normal(loc=0.0, scale=scale, size=size)

d = np.empty((count,))
d[ a &  b] = normal(loc=2, scale=0.3, size=c[ a &  b].shape[0])
d[ a & ~b] = normal(loc=3, scale=0.3, size=c[ a & ~b].shape[0])
d[~a &  b] = normal(loc=4, scale=0.3, size=c[~a &  b].shape[0])
d[~a & ~b] = normal(loc=5, scale=0.3, size=c[~a & ~b].shape[0])

print(pd.DataFrame(dict(a=a, b=b, c=c, d=d)))
