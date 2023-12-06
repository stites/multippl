import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import torch
import pyro
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
from utils import *

def network(npackets):
    packets_left  = pyro.sample("packets_left", dist.Binomial(npackets, 0.5))
    packets_right = npackets - packets_left

    packets_left_lost  = pyro.sample("packets_left_lost", dist.Binomial(packets_left, 0.0001))
    packets_right_lost = pyro.sample("packets_right_lost", dist.Binomial(packets_right, 0.0001))

    packets_left_received = packets_left - packets_left_lost
    packets_right_received = packets_right - packets_right_lost

    left_sent_to_destination = packets_left_received
    right_sent_to_destination = packets_right_received

    destination_lost_left_packets  = pyro.sample("destination_lost_left_packets", dist.Binomial(left_sent_to_destination, 0.0001))
    destination_lost_right_packets = pyro.sample("destination_lost_right_packets", dist.Binomial(right_sent_to_destination, 0.0001))

    destination_received_left  = left_sent_to_destination  - destination_lost_left_packets
    destination_received_right = right_sent_to_destination - destination_lost_right_packets

    return destination_received_left + destination_received_right

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

ising_model = lambda: mkgrid(3, probfn)

def model():
    npackets = pyro.sample("npackets", dist.Poisson(3.0))
    seen = 0
    for _ in range(int(npackets.item())):
        traverses = ising_model()[-1][-1] == 1.
        seen += 1 if traverses else 0
    return seen

for x in range(3):
  print(model())

# def guide():
#     lambda_ = pyro.param("lambda_", torch.tensor(4096.0), constraint=dist.constraints.positive)
#     npackets = pyro.sample("npackets", dist.Poisson(lambda_))
#     return network(npackets)

# # Importance sampling
# num_samples = 100
# importance = Importance(model, guide).run()
# marginal = EmpiricalMarginal(importance, sites=["npackets"])

# # Estimate the expectation
# samples = [marginal().item() for _ in range(num_samples)]

# print("Expected packets: ", sum(samples) / num_samples)
