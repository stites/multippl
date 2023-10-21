import torch
import pyro
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal

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

def model():
    npackets = pyro.sample("npackets", dist.Poisson(4096))
    return network(npackets)

def guide():
    lambda_ = pyro.param("lambda_", torch.tensor(4096.0), constraint=dist.constraints.positive)
    npackets = pyro.sample("npackets", dist.Poisson(lambda_))
    return network(npackets)

# Importance sampling
num_samples = 100
importance = Importance(model, guide).run()
marginal = EmpiricalMarginal(importance, sites=["npackets"])

# Estimate the expectation
samples = [marginal().item() for _ in range(num_samples)]

print("Expected packets: ", sum(samples) / num_samples)
