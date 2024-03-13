# import os, sys
# sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import torch
import pyro
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
import numpy as np
import random
import pyro
import time
#from utils import *

truth = [ 0.956412014 * 3.0 ]

def network(suffix=""):
    def flip(n, p):
        return pyro.sample(n+suffix, dist.Bernoulli(p))
    def discrete(n, *ps):
        return pyro.sample(n+suffix, dist.Categorical(torch.tensor([p for p in ps])))


    # Chicago (root)
    arrives_at_chicago = True
    chicago = discrete("chicago", 1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0)
    chicago_to_ashburn = flip("chicago_to_ashburn", reliability) if (arrives_at_chicago and chicago == 0) else False
    chicago_to_minneap = flip("chicago_to_minneap", reliability) if (arrives_at_chicago and chicago == 1) else False
    chicago_to_denver  = flip("chicago_to_denver", reliability) if (arrives_at_chicago and chicago == 2) else False

    # Dallas (root)
    arrives_at_dallas = True
    dallas = flip("dallas", 0.5)
    dallas_to_denver  = flip("dallas_to_denver", reliability) if (arrives_at_dallas and  dallas) else False
    dallas_to_pheonix = flip("dallas_to_pheonix", reliability) if (arrives_at_dallas and not dallas) else False

    # St Cloud (root)
    arrives_at_stcloud = True
    stcloud = flip("stcloud", 0.5)
    stcloud_to_fargo   = flip("stcloud_to_fargo", reliability) if arrives_at_stcloud and  stcloud else False
    stcloud_to_minneap = flip("stcloud_to_minneap", reliability) if arrives_at_stcloud and not stcloud else False

    # Minneapolis
    arrives_at_minneap = stcloud_to_minneap or chicago_to_minneap
    minneap = discrete("minneap", 1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0)
    minneap_to_fargo   = flip("minneap_to_fargo", reliability) if arrives_at_minneap and minneap == 0 else False
    minneap_to_seattle = flip("minneap_to_seattle", reliability) if arrives_at_minneap and minneap == 1 else False
    minneap_to_newyork = flip("minneap_to_newyork", reliability) if arrives_at_minneap and minneap == 2 else False

    # Los Angeles (root)
    arrives_at_losangeles = True
    losangeles = discrete("losangeles", 1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0)
    losangeles_to_santaclara = flip("losangeles_to_santaclara", reliability) if arrives_at_losangeles and losangeles == 0 else False
    losangeles_to_lasvegas   = flip("losangeles_to_lasvegas", reliability) if arrives_at_losangeles and losangeles == 1 else False
    losangeles_to_phoenix    = flip("losangeles_to_phoenix", reliability) if arrives_at_losangeles and losangeles == 2 else False

    # Santa Clara
    arrives_at_santaclara = losangeles_to_santaclara
    santaclara = True  # is deterministic
    santaclara_to_sacramento = flip("santaclara_to_sacramento", reliability) if arrives_at_santaclara else False

    # LasVegas
    arrives_at_lasvegas = losangeles_to_lasvegas
    lasvegas = flip("lasvegas", 0.5)
    lasvegas_to_saltlake = flip("lasvegas_to_saltlake", reliability) if arrives_at_lasvegas and  lasvegas else False
    lasvegas_to_pheonix  = flip("lasvegas_to_pheonix", reliability) if arrives_at_lasvegas and not lasvegas else False

    # Sacramento
    arrives_at_sacramento = santaclara_to_sacramento
    sacramento = discrete("sacramento", 1.0 / 4.0, 1.0 / 4.0, 1.0 / 4.0, 1.0 / 4.0)
    sacramento_to_santarose = flip("sacramento_to_santarose", reliability) if arrives_at_sacramento and sacramento == 0 else False
    sacramento_to_reno      = flip("sacramento_to_reno", reliability) if arrives_at_sacramento and sacramento == 1 else False
    sacramento_to_rancho    = flip("sacramento_to_rancho", reliability) if arrives_at_sacramento and sacramento == 2 else False
    sacramento_to_portland  = flip("sacramento_to_portland", reliability) if arrives_at_sacramento and sacramento == 3 else False

    # Salem (root)
    arrives_at_salem = True
    salem = flip("salem", 0.5)
    salem_to_portland = flip("salem_to_portland", reliability) if arrives_at_salem and  salem else False
    salem_to_eugene   = flip("salem_to_eugene", reliability) if arrives_at_salem and not salem else False

    # Bend (root)
    arrives_at_bend = True
    bend = discrete("bend", 1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0)
    bend_to_portland = flip("bend_to_portland", reliability) if arrives_at_bend and bend == 0 else False
    bend_to_seattle  = flip("bend_to_seattle", reliability) if arrives_at_bend and bend == 1 else False
    bend_to_saltlake = flip("bend_to_saltlake", reliability) if arrives_at_bend and bend == 2 else False

    # New York (root, determistic)
    arrives_at_newyork = True
    newyork = True
    newyork_to_ashburn = flip("newyork_to_ashburn", reliability) if arrives_at_newyork and newyork else False

    # Spokane (root)
    arrives_at_spokane = True
    spokane = flip("spokane", 0.5)
    spokane_to_seattle  = flip("spokane_to_seattle", reliability) if arrives_at_spokane and  spokane else False
    spokane_to_billings = flip("spokane_to_billings", reliability) if arrives_at_spokane and not spokane else False

    # Billings (deterministic)
    arrives_at_billings = spokane_to_billings
    billings = True
    billings_to_denver = flip("billings_to_denver", reliability) if arrives_at_billings and billings else False

    # Denver (determistic)
    arrives_at_denver = billings_to_denver or dallas_to_denver or chicago_to_denver
    denver = True
    denver_to_ogden = flip("denver_to_ogden", reliability) if arrives_at_denver and denver else False

    # Ogden
    arrives_at_ogden = denver_to_ogden
    ogden = flip("ogden", 0.5)
    ogden_to_orem     = flip("ogden_to_orem", reliability) if arrives_at_ogden and  ogden else False
    ogden_to_saltlake = flip("ogden_to_saltlake", reliability) if arrives_at_ogden and not ogden else False

    # Orem (determistic)
    arrives_at_orem = ogden_to_orem
    orem = True
    orem_to_saltlake = flip("orem_to_saltlake", reliability) if arrives_at_orem and orem else False

    # SaltLakeCity (determistic)
    arrives_at_saltlake = orem_to_saltlake or ogden_to_saltlake or bend_to_saltlake or lasvegas_to_saltlake
    saltlake = True
    saltlake_to_boise = flip("saltlake_to_boise", reliability) if arrives_at_saltlake and saltlake else False

    # Boise (determistic)
    arrives_at_boise = saltlake_to_boise
    boise = True
    boise_to_portland = flip("boise_to_portland", reliability) if arrives_at_boise and boise else False

    # Portland (determistic)
    arrives_at_portland = bend_to_portland or boise_to_portland or sacramento_to_portland
    portland = True
    portland_to_seattle = flip("portland_to_seattle", reliability) if arrives_at_portland and portland else False

    return arrives_at_portland


reliability = 0.999

def model():
    npackets = pyro.sample("npackets", dist.Poisson(3))
    arrives = 0.0
    for ix in pyro.plate("packet", int(npackets.item())+1):
        o = network(suffix=f"_{ix}")
        arrives += o
    return arrives

def model_with_obs():
    npackets = pyro.sample("npackets", dist.Poisson(3))
    arrives = 0.0
    for ix in pyro.plate("packet", int(npackets.item())+1):
        o = pyro.condition(network, {f"dallas_{ix}": torch.zeros(1)})(suffix=f"_{ix}")
        arrives += o
    return arrives


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,) # no-op
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    # we are benchmarkg, expect the same output as yodel
    torch.manual_seed(args.seed)
    np.random.seed(args.seed)
    random.seed(args.seed)

    start = time.time()
    importance = Importance(model, num_samples=args.num_samples)
    posterior = importance.run()
    xs = [sum([tr.nodes["_RETURN"]["value"] for tr in importance.exec_traces]) / args.num_samples]
    end = time.time()

    s = end - start
    print(" ".join([f"{x}" for x in xs]))
    print("{:.3f}ms".format(s * 1000))
