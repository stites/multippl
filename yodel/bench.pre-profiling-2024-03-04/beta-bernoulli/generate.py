import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from util import as_json

import random
import numpy as np
import pyro
import pyro.distributions as dist
from numpy.random import normal
import torch

pyro.set_rng_seed(3)

count = 1000

with pyro.plate("data", count):
    xs = pyro.sample("xs", dist.Bernoulli(0.75)).bool()

if __name__ == "__main__":
    keys = as_json(xs, batch_size=5, outname="data", outext="json", col_names=["xs"])
    print(sorted(list(keys)))
