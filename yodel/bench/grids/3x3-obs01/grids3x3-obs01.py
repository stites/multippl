import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

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
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    model = lambda: mkgrid(3, probfn)
    model = pyro.condition(model, data={"flip01": torch.tensor(0.0)})

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
