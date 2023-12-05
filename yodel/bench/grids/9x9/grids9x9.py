from utils import *
from yodel import sites9, truth9

def probfn(i, j):
   match (i, j):
       case (0, 0): return [0.5]
       case (_, 0): return [1./5, 1./6]
       case (0, _): return [1./3, 1./4]
       case (1, _): return [1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0]
       case (2, _): return [2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0]
       case (3, _): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
       case (4, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case (5, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case (6, _): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
       case (7, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case (8, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case _: raise Exception()



if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=10_000, type=int,)
    parser.add_argument("--num-runs", default=10, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    ising_model = lambda: mkgrid(9, probfn)

    (l1s, times) = runall(ising_model, sites9, truth9, num_runs=args.num_runs, num_samples=args.num_samples, start_seed=args.seed)
    print("--------")
    runs = len(l1s)
    print(f"averages over {runs} runs:")
    print("wallclock:", sum(times) / len(times), "s")
    print("       L1:", sum(l1s) / len(l1s))

    # averages over 10 runs:
    # wallclock: 296.45508604049684 s
    #        L1: 4.415222655601883
