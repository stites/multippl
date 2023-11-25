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

import json
from collections import namedtuple
import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from util import as_json



torch.set_printoptions(sci_mode=False, precision=3, linewidth=120)

def indexes(matrix):
    return [(i, j) for i in range(matrix.shape[0]) for j in range(matrix.shape[1])]

transitions = torch.tensor([
    [0.3, 0.6, 0.1, 0.0],          # initial state
    [0.6, 0.1, 0.3, 0.0],
    [0.25, 0.50, 0.05, 0.20],
    [0, 0, 0, 0],                   # terminal state
])

state_shift = 65
obs_shift = 97
transition_names = list(map(lambda ft: chr(ft[0]+state_shift) + "2" +chr(ft[1]+state_shift), indexes(transitions)))

emissions = torch.tensor([
    [0/3, 2/3, 1/3],
    [3/6, 2/6, 1/6],
    [1/12, 1/12, 10/12],
    [0, 0, 0],                       # terminal state
])

data_dim = emissions.shape[1]
emission_names = list(map(lambda ft: chr(ft[0]+state_shift) + "_" +chr(ft[1]+obs_shift), indexes(emissions)))

def sequence(num_sequences):
    state = torch.tensor(0)   # start state is always
    observations = []
    iobservations = []
    states = [state]
    max_sequence = 0

    state_seqs = []
    seq_lengths = []
    obs_seqs = []
    iobs_seqs = []

    for i in pyro.plate("sequences", num_sequences):
        while state != 3:
            obs = pyro.sample("obs", Categorical(emissions[state]))
            iobservations.append(obs)
            o = one_hot(obs, num_classes=3)
            observations.append(o)
            state = pyro.sample("state", Categorical(transitions[state]))
            states.append(state)
        obs_seqs.append(observations)
        iobs_seqs.append(iobservations)
        states = states[:-1]
        assert 3 not in states, "terminal state included in state transitions"
        state_seqs.append(states)
        seq_lengths.append(len(states))
        max_sequence = seq_lengths[-1] if seq_lengths[-1] > max_sequence else max_sequence

        observations = []
        iobservations = []
        state = torch.tensor(0)
        states = [state]

    state_sequences = (-1) * torch.ones(num_sequences, max_sequence, 1).long()
    sequences = (-1) * torch.ones(num_sequences, max_sequence, data_dim).long()
    isequences = (-1) * torch.ones(num_sequences, max_sequence, 1).long()
    for (i, (ss, os, ios)) in enumerate(zip(state_seqs, obs_seqs, iobs_seqs)):
        state_sequences[i, :len(ss), :] = torch.vstack(ss)
        sequences[i, :len(os), :] = torch.vstack(os)
        isequences[i, :len(ios), :] = torch.vstack(ios)
    sequence_lengths = torch.tensor(seq_lengths).long()

    return state_sequences, sequences, isequences, sequence_lengths

def pyro_data(args):
    print("generating pyro dataset with {} sequences".format(args.num_sequences))
    states, seqs, _, seq_lens = sequence(args.num_sequences)
    d = dict(states=states, seqs=seqs, seq_lens=seq_lens)
    torch.save(d, "hmm0.{}.pt".format(args.seed))

def yodel_data(args):
    print("generating yodel dataset with {} sequences in {}-sized batches".format(args.num_sequences, args.batch_size))
    assert args.num_sequences % args.batch_size == 0, f"num sequences must be divisible by batch size, got num sequences: {args.num_sequences} and batch size {args.batch_size}"
    states, _, iseqs, seq_lens = sequence(args.num_sequences)
    _, _, iseqs, seq_lens = sequence(args.num_sequences)

    d = dict(states=states + 1, seqs=iseqs + 1, seq_lens=seq_lens) # states and seqs are 1-indexed in main.yo
    keys = as_json(d['seqs'], d['seq_lens'], batch_size=args.batch_size, outname="data", outext="json", col_names=['seqs', 'lens'], seq_lens=seq_lens)
    print(sorted(list(keys)))

if __name__ == "__main__":
    import sys
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("format", type=str)
    bs = 4
    ns = bs * 10
    parser.add_argument("-b", "--batch-size", default=bs, type=int,)
    parser.add_argument("-n", "--num-sequences", default=ns, type=int,)
    parser.add_argument("-s", "--seed", default=0, type=int,)
    #parser.add_argument("--len_prefix", default="len", type=str)
    #parser.add_argument("--obs_prefix", default="data", type=str)

    args = parser.parse_args()
    pyro.set_rng_seed(args.seed)
    match args.format:
        case "pyro":
            pyro_data(args)
        case "yodel":
            yodel_data(args)

        case "all":
            print("generating both python and yodel data")
            pyro_data(args)
            yodel_data(args)

        case x:
            print("must be one of 'pyro', 'yodel', or 'all'. Got: '{}'".format(x), file=sys.stderr);
            sys.exit(1)
