import pyro
import itertools
from abc import abstractmethod
import time

import torch
import torch.nn as nn
from torch.distributions import constraints

import pyro
import pyro.poutine as poutine
import pyro.distributions as dist
from pyro.distributions.torch import Categorical
from pyro.distributions.torch_distribution import TorchDistribution
from pyro.infer import SVI, Trace_ELBO, TraceEnum_ELBO, config_enumerate, infer_discrete, EmpiricalMarginal
import pyro.distributions as dist
from pyro.infer.importance import Importance
from torch.nn.functional import one_hot

import sys
import random
import json
import pandas as pd
import numpy as np
from pyro.infer.mcmc import MCMC, NUTS

seed = 0
pyro.set_rng_seed(seed)
torch.set_printoptions(sci_mode=False, precision=3, linewidth=120)

from generate import transitions, emissions, transition_names, emission_names
ds = torch.load(f"./hmm0.{seed}.pt")

states, sequences, lengths = (ds['states'], ds['seqs'], ds['seq_lens'])

print("sequence length of head:", lengths[0].item())
print("   head states.shape = ", states[0][:lengths[0]].squeeze(-1).shape)
print("head sequences.shape = ", sequences[0][:lengths[0]].squeeze(-1).shape)

true_transitions = transitions.flatten()
transition_sites = transition_names
true_emissions = emissions.flatten()
emission_sites = emission_names

#####################################################################################################

# The latent state is x, and the observed state is y. We'll drive
# probs_* with the guide, enumerate over x, and condition on y.
@config_enumerate
def model(sequences, lengths, args, include_prior=True, batch_size=None):
    state_size, obs_size = args["state_size"], args["obs_size"]
    # num_sequences, max_length, data_dim = sequences.shape
    num_sequences, max_length, data_dim = sequences.shape

    #with poutine.mask(mask=include_prior):

    # Our prior on transition probabilities will be:
    # stay in the same state with 50% probability; uniformly jump to another
    # state with 25% probability.
    probs_x = pyro.sample(
        "probs_x",
        dist.Dirichlet(0.5 * torch.eye(state_size) + 0.25).to_event(1), # to_event means "treat these dimensions as a single event"
    )
    # put uniform prior on the observed sequence
    probs_y = pyro.sample(
        "probs_y",
        dist.Beta(1.0, 1.0).expand([state_size, data_dim]).to_event(2),
    )

    # In this first model we'll sequentially iterate over sequences in a
    # minibatch; this will make it easy to reason about tensor shapes.
    data_plate = pyro.plate("observations", data_dim, dim=-1)
    for i in pyro.plate("sequences", len(sequences)):
        length = lengths[i]
        sequence = sequences[i, :length]
        x = 0
        for t in pyro.markov(range(length)):
            # On the next line, we'll overwrite the value of x with an updated
            # value. If we wanted to record all x values, we could instead
            # write x[t] = pyro.sample(...x[t-1]...).
            x = pyro.sample(
                "x_{}_{}".format(i, t),
                dist.Categorical(probs_x[x]),
                infer={"enumerate": "parallel"},
            )
            with data_plate:
                pyro.sample(
                    "y_{}_{}".format(i, t),
                    dist.Categorical(probs_y[x.squeeze(-1)]),
                    obs=sequence[t],
                )

args = dict(state_size=3, obs_size=3)

num_observations = float(lengths.sum())
num_samples = 5
sites=["probs_x", "probs_y"]

importance = Importance(model, guide=None, num_samples=num_samples)

print("running importance sampling...")
start = time.time()
emp_marginal = EmpiricalMarginal(importance.run(sequences, lengths, args), sites=sites)
end = time.time()

[posterior_marg_probs_x, posterior_marg_probs_y] = [emp_marginal.mean[mn] for mn in range(len(sites))]
lws = torch.vstack(importance.log_weights)

torch.set_printoptions(sci_mode=False, precision=3, linewidth=120)

print("post transitions:")
print(posterior_marg_probs_x)
print("true transitions:")
print(transitions[:-1,:-1])
print("  L1 transitions: {:.3f}".format((posterior_marg_probs_x - transitions[:-1,:-1]).abs().sum().item()))
print()

print("post emissions:")
print(posterior_marg_probs_y)
print("true emissions:")
print(emissions[:-1, :])
print("  L1 emissions: {:.3f}".format((posterior_marg_probs_y - emissions[:-1, :]).abs().sum().item()))
print()
print("           ESS: {:.3f} / {}".format(importance.get_ESS().item(), num_samples))
print("   Min/Max lw: {:.3f} <= log(w) <= {:.3f}".format(lws.min().item(), lws.max().item()))
print("     E[log(w)]: {:.3f}".format(lws.mean().item()))
print("   Var[log(w)]: {:.3f}".format(lws.std().item()))
print("-----------------------------------------")
print("  wallclock: {:.3f}s".format(end - start))
print()
