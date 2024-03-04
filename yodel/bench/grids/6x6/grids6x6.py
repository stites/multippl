import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


from utils import *
from yodel import sites6, truth6

def model():
    '''
      00 - 01 - 02 - 03 - 04 - 05
      10 - 11 - 12 - 13 - 14 - 15
      20 - 21 - 22 - 23 - 24 - 25
      30 - 31 - 32 - 33 - 34 - 35
      40 - 41 - 42 - 43 - 44 - 45
      50 - 51 - 52 - 53 - 54 - 55
    '''
    x00 = flip("00", 1.0 / 2.0)

    x01 = edge("01", x00, 1.0 / 3.0, 1.0 / 4.0)
    x02 = edge("02", x01, 1.0 / 3.0, 1.0 / 4.0)
    x03 = edge("03", x02, 1.0 / 3.0, 1.0 / 4.0)
    x04 = edge("04", x03, 1.0 / 3.0, 1.0 / 4.0)
    x05 = edge("05", x04, 1.0 / 3.0, 1.0 / 4.0)

    x10 = edge("10", x00, 1.0 / 5.0, 1.0 / 6.0)
    x20 = edge("20", x10, 1.0 / 5.0, 1.0 / 6.0)
    x30 = edge("30", x20, 1.0 / 5.0, 1.0 / 6.0)
    x40 = edge("40", x30, 1.0 / 5.0, 1.0 / 6.0)
    x50 = edge("50", x40, 1.0 / 5.0, 1.0 / 6.0)

    x11 = node("11", x10, x01, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x12 = node("12", x11, x02, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x13 = node("13", x12, x03, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x14 = node("14", x13, x04, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x15 = node("15", x14, x05, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)

    x21 = node("21", x20, x11, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x22 = node("22", x21, x12, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x23 = node("23", x22, x13, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x24 = node("24", x23, x14, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x25 = node("25", x24, x15, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)

    x31 = node("31", x30, x21, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x32 = node("32", x31, x22, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x33 = node("33", x32, x23, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x34 = node("34", x33, x24, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x35 = node("35", x34, x25, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)

    x41 = node("41", x40, x31, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x42 = node("42", x41, x32, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x43 = node("43", x42, x33, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x44 = node("44", x43, x34, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x45 = node("45", x44, x35, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)

    x51 = node("51", x50, x41, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x52 = node("52", x51, x42, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x53 = node("53", x52, x43, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x54 = node("54", x53, x44, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)
    x55 = node("55", x54, x45, 1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0)

    return [[x00, x01, x02, x03, x04, x05],
            [x10, x11, x12, x13, x14, x15],
            [x20, x21, x22, x23, x24, x25],
            [x30, x31, x32, x33, x34, x35],
            [x40, x41, x42, x43, x44, x45],
            [x50, x51, x52, x53, x54, x55]]

# def probfn(i, j):
#    match (i, j):
#        case (0, 0): return [0.5]
#        case (_, 0): return [1./5, 1./6]
#        case (0, _): return [1./3, 1./4]
#        case (1, _): return [1.0 / 7.0, 1.0 / 8.0, 1.0 / 9.0, 1.0 / 11.0]
#        case (2, _): return [2.0 / 7.0, 2.0 / 8.0, 2.0 / 9.0, 2.0 / 11.0]
#        case (3, _): return [6.0 / 7.0, 6.0 / 8.0, 6.0 / 9.0, 6.0 / 11.0]
#        case (4, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
#        case (5, _): return [3.0 / 7.0, 3.0 / 8.0, 8.0 / 9.0, 9.0 / 11.0]
#        case _: raise Exception()

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=10, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()


    # model = lambda: mkgrid(6, probfn)

    if args.num_runs > 1:
        (l1s, times) = runall(model, sites6, truth6, num_runs=args.num_runs, num_samples=args.num_samples, start_seed=args.seed)
        print("--------")
        runs = len(l1s)
        print(f"averages over {runs} runs:")
        print("wallclock:", sum(times) / len(times), "s")
        print("       L1:", sum(l1s) / len(l1s))
    else:
        # we are benchmarking, expect the same output as yodel
        torch.manual_seed(args.seed)
        np.random.seed(args.seed)
        random.seed(args.seed)
        start = time.time()
        importance = Importance(model, num_samples=args.num_samples)
        posterior = importance.run()
        xs = allmarg(posterior, sites6, num_samples=args.num_samples)
        end = time.time()
        s = end - start
        print(" ".join([f"{x}" for x in xs]))
        print("{:.3f}ms".format(s * 1000))
        # if s < 1.0:
        #     print("{:.3f}ms".format(s / 1000))
        # else:
        #     print("{:.3f}s".format(s))
