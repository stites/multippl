import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils import *

truth = [0.19243187411660567]

def model():
    x00 = flip("00", 1.0 / 2.0)
    x01 = edge("01", x00, 1.0 / 3.0, 1.0 / 4.0)
    x02 = edge("02", x01, 1.0 / 3.0, 1.0 / 4.0)
    x10 = edge("10", x00, 1.0 / 5.0, 1.0 / 6.0)
    x20 = edge("20", x10, 1.0 / 5.0, 1.0 / 6.0)

    x21 = edge("21", x20, 0.25, 0.18181818181818182)

    x12 = edge("12", x02, 0.8571428571428571, 0.5454545454545454)

    x22 = node("22", x21, x12, 3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0)
    return x20

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    model = pyro.condition(model, data={"flip22": torch.tensor(0.0)})

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
