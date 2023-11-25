from utils import *
from yodel import sites3, truth3

def ising_model():
    x00 = flip("00", 1.0 / 2.0)
    x01 = edge("01", x00, 1.0 / 3.0, 1.0 / 4.0)
    x02 = edge("02", x01, 1.0 / 3.0, 1.0 / 4.0)
    x10 = edge("10", x00, 1.0 / 5.0, 1.0 / 6.0)
    x20 = edge("20", x10, 1.0 / 5.0, 1.0 / 6.0)

    x11 = node("11", x10, x01, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x21 = node("21", x20, x11, 2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0)
    x12 = node("12", x11, x02, 6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0)
    x22 = node("22", x21, x12, 3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0)
    return [[x00, x01, x02],
            [x10, x11, x12],
            [x20, x12, x22]]

def probfn(i, j):
   match (i, j):
       case (0, 0): return [0.5]
       case (0, _): return [1./3, 1./4]
       case (_, 0): return [1./5, 1./6]
       case (1, 1): return [1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0]
       case (2, 1): return [2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0]
       case (1, 2): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
       case (2, 2): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
       case _: raise Exception

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=10_000, type=int,)
    parser.add_argument("--num-runs", default=10, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    ising_model = lambda: mkgrid(3, probfn)

    (l1s, times) = runall(ising_model, sites3, truth3, num_runs=args.num_runs, num_samples=args.num_samples, start_seed=args.seed)
    print("--------")
    runs = len(l1s)
    print(f"averages over {runs} runs:")
    print("wallclock:", sum(times) / len(times), "s")
    print("       L1:", sum(l1s) / len(l1s))

    # averages over 10 runs:
    # wallclock: 42.63511703014374 s
    #        L1: 0.41128323336703865
