#!/usr/bin/env python3
import os, sys
import fileinput
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(os.getcwd())
from main import truth

def compute_l1(ms, truth):
    return list(map(lambda x: abs(x[0] - x[1]), zip(ms, truth)))

if __name__ == "__main__":
    l1 = 0.0
    for line in fileinput.input():
        line = line.rstrip()
        if l1 == 0.0:
            out = [float(f) for f in line.split()]
            print(*out)
            l1 = sum(compute_l1(out, truth))
            print(f"L1    {l1:.3f}")
            print("truth", truth)
        else:
            ms = float(line[:-2])
            s = ms / 1000
            print("-       1000 samples = {:>7.3f} sec".format(s))
            print("-   10 x 100 samples = {:>7.1f} min".format(s*10 / 60  ))
            print("-  100 x  10 samples = {:>7.1f} min".format(s*100 / 60 ))
            print("- 1000 x   1 sample  = {:>7.1f} min".format(s*1000 / 60))
else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
