import importlib.machinery
import pathlib

h0generate = importlib.machinery.SourceFileLoader(
    "hybrid_0_generate",
    pathlib.Path(__file__).parent.joinpath("hybrid.0.generate.py").resolve().as_posix(),
).load_module()
print(h0generate.df)

import os
import torch
import pyro
import pyro.distributions as dist
from torch.distributions import constraints
from pyro import poutine
from pyro.infer import SVI, Trace_ELBO, TraceEnum_ELBO, config_enumerate, infer_discrete
from pyro.infer.autoguide import AutoNormal
from pyro.ops.indexing import Vindex

pyro.set_rng_seed(0)

# @config_enumerate
# def model():
#     p = pyro.param("p", torch.randn(3, 3).exp(), constraint=constraints.simplex)
#     x = pyro.sample("x", dist.Categorical(p[0]))
#     y = pyro.sample("y", dist.Categorical(p[x]))
#     z = pyro.sample("z", dist.Categorical(p[y]))
#     print(f"  model x.shape = {x.shape}")
#     print(f"  model y.shape = {y.shape}")
#     print(f"  model z.shape = {z.shape}")
#     print(f"{x}, {y}, {z}")
#     return x, y, z

# def guide():
#     pass

# pyro.clear_param_store()
# print("Sampling:")
# model()
# print("Enumerated Inference:")
# elbo = TraceEnum_ELBO(max_plate_nesting=0)
# elbo.loss(model, guide);

# serving_model = infer_discrete(model, first_available_dim=-1)
# x, y, z = serving_model()  # takes the same args as model(), here no args
# print(f"x = {x}")
# print(f"y = {y}")
# print(f"z = {z}")


# @config_enumerate
# def model():
#     p = pyro.param("p", torch.randn(5, 4, 3, 2).exp(), constraint=constraints.simplex)
#     x = pyro.sample("x", dist.Categorical(torch.ones(4)))
#     y = pyro.sample("y", dist.Categorical(torch.ones(3)))
#     with pyro.plate("z_plate", 5):
#         p_xy = Vindex(p)[..., x, y, :]
#         z = pyro.sample("z", dist.Categorical(p_xy))
#     print(f"     p.shape = {p.shape}")
#     print(f"     x.shape = {x.shape}")
#     print(f"     y.shape = {y.shape}")
#     print(f"  p_xy.shape = {p_xy.shape}")
#     print(f"     z.shape = {z.shape}")
#     return x, y, z

# def guide():
#     pass

# pyro.clear_param_store()
# print("Sampling:")
# model()
# print("Enumerated Inference:")
# elbo = TraceEnum_ELBO(max_plate_nesting=1)
# elbo.loss(model, guide);

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
@config_enumerate
def model():
    data_plate = pyro.plate("data_plate", 6, dim=-1)
    feature_plate = pyro.plate("feature_plate", 5, dim=-2)
    component_plate = pyro.plate("component_plate", 4, dim=-1)
    with feature_plate:
        with component_plate:
            p = pyro.sample("p", dist.Dirichlet(torch.ones(3)))
    with data_plate:
        c = pyro.sample("c", dist.Categorical(torch.ones(4)))
        with feature_plate as vdx:                # Capture plate index.
            print(vdx[..., None])
            pc = Vindex(p)[vdx[..., None], c, :]  # Reshape it and use in Vindex.
            print(pc.shape)
            x = pyro.sample("x", dist.Categorical(pc),
                            obs=torch.zeros(5, 6, dtype=torch.long))
    print(f"    p.shape = {p.shape}")
    print(f"    c.shape = {c.shape}")
    print(f"  vdx.shape = {vdx.shape}")
    print(f"    pc.shape = {pc.shape}")
    print(f"    x.shape = {x.shape}")

def guide():
    feature_plate = pyro.plate("feature_plate", 5, dim=-2)
    component_plate = pyro.plate("component_plate", 4, dim=-1)
    with feature_plate, component_plate:
        pyro.sample("p", dist.Dirichlet(torch.ones(3)))

pyro.clear_param_store()
print("Sampling:")
model()
print("Enumerated Inference:")
elbo = TraceEnum_ELBO(max_plate_nesting=2)
elbo.loss(model, guide);

################################################################################################
################################################################################################
################################################################################################
################################################################################################

import sys
sys.exit(0)

################################################################################################
################################################################################################
################################################################################################
################################################################################################


import logging
import os
from collections import namedtuple

import torch

import pyro
import pyro.distributions as dist
from pyro.infer.importance import Importance
from pyro.infer.mcmc import NUTS
from pyro.infer.mcmc.api import MCMC
from pyro.infer.mcmc.hmc import HMC


import pyro
import pyro.distributions as dist
from pyro import poutine
from pyro.infer import SVI, JitTraceEnum_ELBO, TraceEnum_ELBO, TraceTMC_ELBO
from pyro.infer.autoguide import AutoDelta
from pyro.ops.indexing import Vindex
from pyro.optim import Adam
import pyro.optim as optim
from pyro.util import ignore_jit_warnings
from pyro.infer import EmpiricalMarginal

# config_enumerate does not work with importance sampling, This was proposed as an unofficial alternative:
# https://forum.pyro.ai/t/confused-about-importance-sampling-result-for-a-gmm/516/5
class EnumImportance(Importance):
    def _traces(self, *args, **kwargs):
        loss = pyro.infer.TraceEnum_ELBO()
        for i in range(self.num_samples):
            model_trace, guide_trace = next(loss._get_traces(self.model, self.guide, args, kwargs))
            log_weight = pyro.infer.traceenum_elbo._compute_dice_elbo(model_trace, guide_trace)
            yield (model_trace, log_weight)


K = 2

@config_enumerate(default="parallel")
def gmm(data):
    mix_proportions = pyro.sample("phi", dist.Dirichlet(torch.ones(K)))
    with pyro.plate("num_clusters", K):
        cluster_means = pyro.sample("cluster_means",
                                    dist.Normal(torch.arange(float(K)), 1.))
    with pyro.plate("data", data.shape[0]):
        assignments = pyro.sample("assignments", dist.Categorical(mix_proportions))
        pyro.sample("obs", dist.Normal(cluster_means[assignments], 1.), obs=data)
    return cluster_means

# Generate data
N = 100
true_cluster_means = torch.tensor([1., 5.])
true_mix_proportions = torch.tensor([0.4, 0.6])
cluster_assignments = dist.Categorical(true_mix_proportions).sample(torch.Size((N,)))
data = dist.Normal(true_cluster_means[cluster_assignments], 1.0).sample()
def gmm_guide(data):
    weight_param = pyro.param("weight_param", torch.ones(K),
                              constraint=constraints.positive)
    mix_proportions = pyro.sample("phi", dist.Dirichlet(weight_param))

    # Fine-tuned initializations
    mean_param = pyro.param("mean_param", torch.tensor([0.9, 3.1]))
    scale_param = pyro.param("scale_param", 0.5*torch.ones(K),
                             constraint=constraints.positive)
    with pyro.plate("num_clusters", K):
        cluster_means = pyro.sample("cluster_means", dist.Normal(mean_param, scale_param))

# Fit guide and create posterior
pyro.clear_param_store()
# svi = SVI(model=gmm, guide=gmm_guide,
#           optim=optim.Adam({"lr": 0.01, "betas": [0.8, 0.99]}),
#           loss=TraceEnum_ELBO(max_plate_nesting=1))

# for t in range(3000):
#     svi.step(data)

guided_importance_run = EnumImportance(gmm, gmm_guide, num_samples=50).run(data)
guided_posterior = EmpiricalMarginal(guided_importance_run, sites=["cluster_means"])

import numpy as np
emp_samples = torch.cat([guided_posterior.sample().detach() for _ in range(1000)])
print(emp_samples[:,0].mean())
print(emp_samples[:,0].var())
print(emp_samples[:,1].mean())
print(emp_samples[:,1].var())


################################################################################################
################################################################################################
################################################################################################
################################################################################################

# def model(data):
#     pa = pyro.sample("a", dist.Beta(1.0, 1.0))
#     pbt = pyro.sample("bt", dist.Beta(1.0, 1.0))
#     pbf = pyro.sample("bf", dist.Beta(1.0, 1.0))
#     # pc = pyro.sample("c", dist.Normal(0.0, 1.0))
#     # pd_ab = pyro.sample("d_ab", dist.Normal(2.5, 1.5))
#     # pd_anb = pyro.sample("d_anb", dist.Normal(2.5, 1.5))
#     # pd_nab = pyro.sample("d_nab", dist.Normal(2.5, 1.5))
#     # pd_nanb = pyro.sample("d_nanb", dist.Normal(2.5, 1.5))

#     pyro.sample("obs_a", dist.Bernoulli(pa), obs=data.a)
#     if data.a:
#        pyro.condition(scale, data={"measurement": torch.tensor(9.5)})
#     with pyro.plate("data", len(data)):
#         if data.a
#         pyro.sample("obs_b", dist.Bernoulli(pbt), obs=data.b)
#         pyro.sample("obs_b", dist.Bernoulli(pbf), obs=data.b)
