#!/usr/bin/env python3
import os, sys
import fileinput
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
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
            print("L1", l1)
        else:
            ms = float(line[:-2])
            s = ms / 1000
            print(line, "=", s, "s =[x1000 samples / 3 epyc speedup]=>", "{:.4f} min".format((s * 1000) / 60 / 3))
else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
