#!/usr/bin/env python3
from numpy import genfromtxt
import numpy as np

import os
import sys
tmpl3x3="grid3x3-approx-dZero-n1000000-c100-s___obs"
data = {}
if __name__ == "__main__":
    path = sys.argv[1]
    files = os.listdir(path)
    for fn in files:
        csv = os.path.join(path, fn)
        print(csv, os.path.isfile(csv))
        if os.path.isfile(csv):
            key = fn[len(tmpl3x3):-4]
            print("hello", key)
            dat=genfromtxt(csv, delimiter='\t', skip_header=True, names = ['gridsize', 'comp', 'det', 'micros', 'l1'], usecols = (4), max_rows=1_000_000)
            if len(dat) < 1_000_000:
                print("heads up, data is incomplete:", csv)
            else:
                print(dat[-10:])
                if key in data:
                    data[key] = np.append(np.expand_dims(data[key], 0), dat, axis=0)
                    print(data[key].shape)
                else:
                    data[key] = np.expand_dims(dat, 0)
print("...done")
