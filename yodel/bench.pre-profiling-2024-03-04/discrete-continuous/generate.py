import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from util import as_json
from collections import OrderedDict

import random
import numpy as np
from numpy.random import normal

random.seed(3)
np.random.seed(3)

#count = 1000
count = 5

def flips(p, l):
    return np.array([random.random() < p for _ in range(l)])

def conditional_flips1(ps, cond_on):
    (pt, pf) = ps
    t_ix = np.where(cond_on == True)
    t = flips(pt, t_ix[0].shape[0])
    f_ix = np.where(cond_on == False)
    f = flips(pf, f_ix[0].shape[0])
    nxt = np.empty((cond_on.shape[0],), dtype=bool)
    nxt[t_ix] = t
    nxt[f_ix] = f
    return nxt

def conditional_flips2(ps, cond_on_par1, cond_on_par2):
    (ptt, ptf, pft, pff) = ps
    tt_ix = np.where(np.logical_and(cond_on_par1, cond_on_par2))
    tf_ix = np.where(np.logical_and(cond_on_par1, np.logical_not(cond_on_par2)))
    ft_ix = np.where(np.logical_and(np.logical_not(cond_on_par1), cond_on_par2))
    ff_ix = np.where(np.logical_and(np.logical_not(cond_on_par1), np.logical_not(cond_on_par2)))
    tt = flips(ptt, tt_ix[0].shape[0])
    tf = flips(ptf, tf_ix[0].shape[0])
    ft = flips(pft, ft_ix[0].shape[0])
    ff = flips(pff, ff_ix[0].shape[0])
    nxt = np.empty((cond_on_par1.shape[0],), dtype=bool)
    nxt[tt_ix] = tt
    nxt[tf_ix] = tf
    nxt[ft_ix] = ft
    nxt[ff_ix] = ff
    return nxt

truth = OrderedDict(
    [("a", 0.5),
     ("bt", 1./4),
     ("bf", 3./4),
     ("ct", 7./8),
     ("cf", 1./8),
     ("dtt", 7./8),
     ("dtf", 6./8),
     ("dft", 5./8),
     ("dff", 4./8),
     ("et", 0),
     ("ef", 2),
     ("ft", 2),
     ("ff", 0)
     ])

a = flips(truth['a'], count)
b = conditional_flips1((truth['bt'], truth['bf']), a)
c = conditional_flips1((truth['ct'], truth['cf']), a)
d = conditional_flips2((truth['dtt'], truth['dtf'], truth['dft'], truth['dff']), b, c)

e = np.empty((count,))
e[  b | d ] = normal(loc=truth['et'], scale=0.5, size=d[  b | d ].shape[0])
e[~(b | d)] = normal(loc=truth['ef'], scale=0.5, size=d[~(b | d)].shape[0])

f = np.empty((count,))
f[  c | d ] = normal(loc=truth['ft'], scale=0.5, size=d[  c | d ].shape[0])
f[~(c | d)] = normal(loc=truth['ff'], scale=0.5, size=d[~(c | d)].shape[0])

g = normal(loc=f+e, scale=0.5, size=count)

data = [a, b, c, d, e, f, g]
if __name__ == "__main__":
    for node in data:
        print(node)
    keys = as_json(*data, batch_size=5, outname="data", outext="json", col_names=["a", "b", "c", "d", "e", "f", "g"])
    print(sorted(list(keys)))
