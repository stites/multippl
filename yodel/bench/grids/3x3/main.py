#!/usr/bin/env python3


import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


from utils import *

truth = [0.33334458827669033, 0.21670809411200728, 0.21115938426591044, 0.21679751298423097, 0.12242493857019612, 0.1316326011696978, 0.21238344545358911, 0.14949403961104374, 1]

def probfn(i, j):
   match (i, j):
       case (0, 0): return [1./3]
       case (_, 0): return [1./4, 1./5]
       case (0, _): return [1./4, 1./5]
       case (1, _): return [1.0 / 6.0, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0]
       case (2, _): return [1.0 / 6.0, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0]
       case _: raise Exception()

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    model = lambda: mkgrid(3, probfn)
    model = pyro.condition(model, data={"flip22": torch.tensor(1.0)})

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
        #import main; print(sum(compute_l1(xs, main.truth)))
