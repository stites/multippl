#!/usr/bin/env python3
import pyro
import numpy as np
import random
import time
import torch
from collections import deque

import pyro
import pyro.distributions as dist
from pyro.infer.importance import Importance

num_nodes = 4
num_steps = 3
truth = [ 28 / 9 ]
num_steps = 8
truth = [ 8492 / 2187 ]
def forward(ix, pkt, node):
    s = pyro.sample(
        f"{ix}_n{node}_{pkt}",
        dist.Categorical(torch.ones(num_nodes - 1) / (num_nodes - 1.0))
    )
    return s if s < node else s + 1

def node(ix, pkt, node):
    p1 = forward(ix, pkt, node)
    p2 = forward(ix, pkt + 1, node)
    return (p1, p2)

def network_step(ix, ns, next):
    _ns = []
    for i in range(num_nodes):
        _ns.append(ns[i] or (next == i))
    return (_ns, *node(ix, 0, next))

def as_num(b):
    return 1.0 if b else 0.0

def model():
    ps = node(0, 0, 0)
    indicators = [True] + [False] * (num_nodes - 1)
    q = deque(ps)
    for ix in range(num_steps):
        nxt = q.popleft()
        state = network_step(ix, indicators, nxt)
        (indicators, p1, p2) = state
        q.append(p1)
        q.append(p2)
    return sum([as_num(i) for i in indicators])

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-runs", default=1, type=int,) # unused!
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    # we are benchmarking, expect the same output as yodel
    torch.manual_seed(args.seed)
    np.random.seed(args.seed)
    random.seed(args.seed)
    start = time.time()
    importance = Importance(model, num_samples=args.num_samples)
    importance.run()
    xs = torch.tensor([tr.nodes["_RETURN"]["value"] for tr in importance.exec_traces])
    print(xs.mean().item())
    end = time.time()
    s = end - start
    print("{:.3f}ms".format(s * 1000))
