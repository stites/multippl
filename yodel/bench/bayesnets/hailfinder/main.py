#!/usr/bin/env python3

import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from pyro.infer import Importance, EmpiricalMarginal
import torch
torch.set_num_threads(1)
import pyro
import numpy as np
import random
from pyro.infer import Importance, EmpiricalMarginal
from truth import *
import time

from hailfinder import network

def model():
    evidence = dict(
        R5Fcst = 2,
        WindFieldPln = 0,
    )
    xs = pyro.condition(network, data={k: torch.tensor(v) for k, v in evidence.items()})()
    # return torch.tensor([pyro.sample(f"g{i}", dist.Normal(ls[i]+0.0, 0.5)) for i in range(len(ls))])
    return torch.tensor([x+0.0 for x in xs])

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    if args.num_runs > 1:
        print("not supported")
        sys.exit(1)
    else:
        # we are benchmarking, expect the same output as yodel
        torch.manual_seed(args.seed)
        np.random.seed(args.seed)
        random.seed(args.seed)
        start = time.time()
        importance = Importance(model, num_samples=args.num_samples)
        marginal = EmpiricalMarginal(importance.run())
        xs = marginal.mean.flatten()
        end = time.time()
        s = end - start
        print(" ".join([f"{x}" for x in xs]))
        print("{:.3f}ms".format(s * 1000))
        #print(sum(list(map(lambda x: abs(x[0] - x[1]), zip(xs, truth)))))
