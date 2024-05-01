import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import torch
import pyro
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
from utils import *



import torch
import pyro
import pyro.distributions as dist

import numbers
import warnings
from abc import ABCMeta, abstractmethod
from collections import OrderedDict, defaultdict

import torch
import time
import random
import numpy as np

import pyro.poutine as poutine
from pyro.distributions import Categorical, Empirical
from pyro.distributions.torch_distribution import TorchDistribution
from torch.distributions import constraints



def network(suffix=""):
    def flip(n, p):
        return pyro.sample(n+suffix, dist.Bernoulli(p))
    n30r = True

    n20r = flip("n20r", 1.0 / 2.0) if     n30r else torch.zeros(1)                    # f
    n31r = flip("n31r", 1.0 / 2.0) if not n20r else torch.zeros(1)                    # `-> f

    n10r = flip("n10r", 1.0 / 2.0) if     n20r else torch.zeros(1)                    # -
    n21r = flip("n21r", 1.0 / 2.0) if not n10r else torch.zeros(1)                    # `-> t
    n32r = flip("n32r", 1.0 / 2.0) if     n21r else torch.zeros(1)                    #      `-> t
    n33r = flip("n33r", 1.0 / 2.0) if not n21r else torch.zeros(1)                    # -
    n0   = n10r                                                                       # -
    n10l = flip("n10l", 1.0 / 2.0) if     n0   else torch.zeros(1)                    # --.
                                                                                      #    \
    n20l = flip("n20l", 1.0 / 2.0) if     n10l else torch.zeros(1)                    # -. |
    n21l = flip("n21l", 1.0 / 2.0) if not n10l else torch.zeros(1)                    #  | '-> f
                                                                                      #   \     '-.
    n30l = flip("n30l", 1.0 / 2.0) if     n20l else torch.zeros(1)                    # - |       |
    n31l = flip("n31l", 1.0 / 2.0) if not n20l else torch.zeros(1)                    #   `-> f   |
    n32l = flip("n32l", 1.0 / 2.0) if     n21l else torch.zeros(1)                    # -         |
    n33l = flip("n33l", 1.0 / 2.0) if not n21l else torch.zeros(1)                    #           '->t

    return n0

truth = [0.142857143 * 3] #  32l @ 1/2 = 0.428571429
truth = [0.333333333 * 3] #  33l @ 1/2 = 0.84
truth = [0.115207373 * 3] # !32l @ 1/3 = 0.345622119
truth = [       0.28 * 3] # !32l @ 1/2 = 0.84
truth = [0.225806452 * 3] # !30l @ 1/2 = 0.677419356 <<< XXX
truth = [0.107438017 * 3] # !30l @ 1/3 = 0.322314051 <<< 0.0016mp vs 0.0036py
truth = [0.076923077 * 3] #  32l @ 1/3 =

truth = [0.142857143 * 3] #  32l @ 1/2 = 0.428571429

def model():
    n = 3
    npackets = pyro.sample("npackets", dist.Poisson(n))
    #npackets = torch.ones(1) * n
    #arrives = torch.zeros(n)
    arrives = torch.zeros(1)
    if npackets.item() == 0.0:
        return arrives
    #for ix in pyro.plate("packet", int(npackets.item())):
    for ix in range(int(npackets.item())):
        m = pyro.condition(network, data={f"n32l_{ix}": torch.tensor(1.0)})
        #arrives[ix] = arrives[ix-1] + m(suffix=f"_{ix}").item()
        arrives += m(suffix=f"_{ix}").item()
    return arrives

class EmpiricalMarginal(Empirical):
    """
    Marginal distribution over a single site (or multiple, provided they have the same
    shape) from the ``TracePosterior``'s model.

    .. note:: If multiple sites are specified, they must have the same tensor shape.
        Samples from each site will be stacked and stored within a single tensor. See
        :class:`~pyro.distributions.Empirical`. To hold the marginal distribution of sites
        having different shapes, use :class:`~pyro.infer.abstract_infer.Marginals` instead.

    :param TracePosterior trace_posterior: a ``TracePosterior`` instance representing
        a Monte Carlo posterior.
    :param list sites: optional list of sites for which we need to generate
        the marginal distribution.
    """

    def __init__(self, trace_posterior, sites=None, validate_args=None):
        if sites is None:
            sites = "_RETURN"
        self._samples_buffer = defaultdict(list)
        self._weights_buffer = defaultdict(list)
        self._populate_traces(trace_posterior, sites)
        samples, weights = self._get_samples_and_weights()
        super().__init__(samples, weights, validate_args=validate_args)

    def _get_samples_and_weights(self):
        """
        Appends values collected in the samples/weights buffers to their
        corresponding tensors.
        """
        i = 0

        samples_by_chain = []
        weights_by_chain = []

        samples = torch.stack(self._samples_buffer[i], dim=0)
        samples_by_chain.append(samples)
        weights_dtype = (
            samples.dtype if samples.dtype.is_floating_point else torch.float32
        )
        weights = torch.as_tensor(
            self._weights_buffer[i], device=samples.device, dtype=weights_dtype
        )
        #print(weights.exp())
        weights_by_chain.append(weights)

        if len(samples_by_chain) == 1:
            return samples_by_chain[0], weights_by_chain[0]
        else:
            return torch.stack(samples_by_chain, dim=0), torch.stack(
                weights_by_chain, dim=0
            )

    def _populate_traces(self, trace_posterior, sites):
        assert isinstance(sites, (list, str))
        for tr, log_weight, chain_id in zip(
            trace_posterior.exec_traces,
            trace_posterior.log_weights,
            trace_posterior.chain_ids,
        ):
            value = (
                tr.nodes[sites]["value"]
                if isinstance(sites, str)
                else torch.stack([tr.nodes[site]["value"] for site in sites], 0)
            )
            # Apply default weight of 1.0.
            if log_weight is None:
                log_weight = 0.0
            if (
                self._validate_args
                and not isinstance(log_weight, numbers.Number)
                and log_weight.dim() > 0
               ):
                raise ValueError("``weight.dim() > 0``, but weight should be a scalar.")

            # Append to the buffer list
            self._samples_buffer[chain_id].append(value)
            self._weights_buffer[chain_id].append(log_weight)

class Importance:
    def _reset(self):
        self.num_chains = 1
        self.log_weights = []
        self.exec_traces = []
        self.chain_ids = []  # chain id corresponding to the sample
        self._idx_by_chain = [
            [] for _ in range(self.num_chains)
        ]  # indexes of samples by chain id
        self._categorical = None

    def __call__(self, *args, **kwargs):
        # To ensure deterministic sampling in the presence of multiple chains,
        # we get the index from ``idxs_by_chain`` instead of sampling from
        # the marginal directly.
        random_idx = self._categorical.sample().item()
        chain_idx, sample_idx = (
            random_idx % self.num_chains,
            random_idx // self.num_chains,
        )
        sample_idx = self._idx_by_chain[chain_idx][sample_idx]
        trace = self.exec_traces[sample_idx].copy()
        for name in trace.observation_nodes:
            trace.remove_node(name)
        return trace


    def run(self, *args, **kwargs):
        """
        Calls `self._traces` to populate execution traces from a stochastic
        Pyro model.

        :param args: optional args taken by `self._traces`.
        :param kwargs: optional keywords args taken by `self._traces`.
        """
        self._reset()
        with poutine.block():
            for i, vals in enumerate(self._traces(*args, **kwargs)):
                assert len(vals) == 2
                chain_id = 0
                tr, logit = vals
                ns = tr.nodes
                ks = ns.keys()
                # for k in ks:
                #     if 'log_prob_sum' in ns[k]:
                #         print(k, ns[k]['log_prob_sum'])
                #import pdb; pdb.set_trace();
                self.exec_traces.append(tr)
                self.log_weights.append(logit)
                self.chain_ids.append(chain_id)
                self._idx_by_chain[chain_id].append(i)
        self._categorical = Categorical(logits=torch.tensor(self.log_weights))
        #print(torch.tensor(self.log_weights))
        return self



    def __init__(self, model, guide=None, num_samples=None):
        """
        Constructor. default to num_samples = 10, guide = model
        """
        self._reset()
        if guide is None:
            # propose from the prior by making a guide from the model by hiding observes
            guide = poutine.block(model, hide_types=["observe"])
        #import pdb; pdb.set_trace()
        self.num_samples = num_samples
        self.model = model
        self.guide = guide

    def _traces(self, *args, **kwargs):
        for _ in range(self.num_samples):
            # guide is the unobserved model
            # print("<====[guide]====>")
            guide_trace = poutine.trace(self.guide).get_trace(*args, **kwargs)
            # guide is the observed model
            # print("<====[model]====>")
            model_trace = poutine.trace(
                poutine.replay(self.model, trace=guide_trace)
            ).get_trace(*args, **kwargs)
            #import ipdb; ipdb.set_trace()

            # print("<----[final]---->")
            log_weight = model_trace.log_prob_sum() - guide_trace.log_prob_sum()
            n = guide_trace.nodes['npackets']['value'].int().item()
            #n = 3
            trace = ["x" for _ in range(n-1 if n > 0 else 0)]
            def filt(tr, fn):
                return [fn(k, v) for k, v in tr.nodes.items() if 'value' in v]
            def print_node(n):
                maybeid = lambda k: n[k] if k in n else None
                maybe = lambda k, f: f(n[k]) if k in n else None
                print(f"{n['name']}", n['type'], maybe('fn', lambda fn: fn.__class__.__name__), maybe('value', lambda x: x.item()), maybeid('log_prob_sum'))

            def print_trace(tr):
                for n in tr.nodes.values():
                    print_node(n)

            kv = lambda k, n: (k, n['value'].int().item())
            kvlp = lambda k, n: (k, (n['value'].int().item(), n['log_prob_sum'].exp().item() if 'log_prob_sum' in n else None))

            gtrace_tot = filt(guide_trace, kv)
            gtr = dict(filt(guide_trace, kvlp))
            gkeys = sorted(filt(guide_trace, lambda k, v: k))

            #print(gtrace_tot)
            mtrace_tot = filt(model_trace, kv)
            mtr = dict(filt(model_trace, kvlp))
            mkeys = sorted(filt(guide_trace, lambda k, v: k))
            diff = set(mkeys) - set(gkeys)
            print(diff)

            #print(mtrace_tot)
            #import ipdb; ipdb.set_trace()
            trace_tot = guide_trace.nodes['_RETURN']['value'].int().item() # missing n10r_0 !?!?

            trace.append(str(trace_tot))
            print("{:.5f} @ [{}]".format(log_weight.exp(), ", ".join(trace)))

            ##############################################################################################
            # we are missing nodes???
            ##############################################################################################
            gf = []
            mf = []
            try:
                for ix in range(n):
                    gf.append(guide_trace.nodes[f'n10r_{ix}']['value'])
            except:
                import ipdb; ipdb.set_trace()
            try:
                for ix in range(n):
                    mf.append(model_trace.nodes[f'n10r_{ix}']['value'])
            except:
                import ipdb; ipdb.set_trace()
            """
            observations:
            - it's fine if n10r_? is missing if the function never gets there???
            - if the trace /makes/ it to n32l_? it will be weighted???
              so, for example,

                  0.50000 @ [x, x, x, x, x, 2]

              makes it to n32l_? twice?
            """

            # print(model_trace.log_prob_sum().exp(), '/', guide_trace.log_prob_sum().exp(), '=', log_weight.exp())
            yield (model_trace, log_weight)

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    if args.num_runs > 1:
        print("not supported")
        import sys; sys.exit(1)

    else:
        # we are benchmarking, expect the same output as yodel
        torch.manual_seed(args.seed)
        np.random.seed(args.seed)
        random.seed(args.seed)
        start = time.time()
        importance = Importance(model, num_samples=args.num_samples)
        importance = importance.run()
        marginal = EmpiricalMarginal(importance)
        #import ipdb; ipdb.set_trace()
        import bench
        xs = bench.ismean(marginal).flatten()
        end = time.time()
        s = end - start
        ## print(" ".join([f"|{t:.4f} - {x:.4f}| = {(t - x).abs():.4f}" for x, t in zip(xs, truth)]))
        print(" ".join([str(x.item()) for x in xs]))
        print("{:.3f}ms".format(s * 1000))
        #print(sum(compute_l1(xs, truth)))
