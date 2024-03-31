#!/usr/bin/env python3


import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


from utils import *

truth = [0.3333333333334798, 0.21666666666693185, 0.21083333333628695, 0.2105416666970542, 0.21052708357338137, 0.2105263552795258, 0.21666666666864545, 0.12146990741169122, 0.11815096709146626, 0.11803415720452, 0.11802998801319309, 0.11802987399037639, 0.2108333333676219, 0.11974824105881274, 0.11669456811089668, 0.1165922715378285, 0.11658920539900658, 0.11659171444060949, 0.21054166721211784, 0.11971098889018617, 0.11667143121353572, 0.11657107015988614, 0.11658623963232499, 0.1167668468875766, 0.21052709033990313, 0.11970998674170262, 0.11667288301059903, 0.11661284198717187, 0.11744472859777955, 0.12987517550866529, 0.21052640950027282, 0.11971092554527064, 0.11670250309745635, 0.11752665126655908, 0.14564309311765267, 1.0]

def probfn(i, j):
   match (i, j):
       case (0, 0): return [1./3]
       case (_, 0): return [1./4, 1./5]
       case (0, _): return [1./4, 1./5]
       case (1, _): return [1.0 / 6.0, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0]
       case (2, _): return [1.0 / 6.0, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0]
       case (3, _): return [1.0 / 6.0, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0]
       case (4, _): return [1.0 / 6.0, 1.0 / 7.0, 8.0 / 8.0, 1.0 / 9.0]
       case (5, _): return [1.0 / 6.0, 1.0 / 7.0, 8.0 / 8.0, 1.0 / 9.0]
       case _: raise Exception()

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    model = lambda: mkgrid(6, probfn)
    model = pyro.condition(model, data={"flip55": torch.tensor(1.0)})

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
