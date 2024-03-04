""" Just a test run, so not worrying about parallelism here. """
import time
import torch
import pyro
import pyro.distributions as dist
from pyro.infer import EmpiricalMarginal
from pyro.infer.importance import Importance


def model(count, par=True):
    p = pyro.sample("p", dist.Beta(1.0, 1.0))
    if par:
        with pyro.plate("data", count):
            pyro.sample(f"obs", dist.Bernoulli(p))
    else:
        for i in pyro.plate("data", count):
            pyro.sample(f"obs_{i}", dist.Bernoulli(p))
    return torch.tensor([p])


print("x")
if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser("beta-bernoulli")
    parser.add_argument(
        "--num-samples", help="how many samples to run", default=100, type=int
    )
    parser.add_argument("--seed", help="set seed", default=0, type=int)
    parser.add_argument("--sequential", help="set seed", default=False, type=bool)
    args = parser.parse_args()

    par = not args.sequential
    pyro.set_rng_seed(args.seed)

    from generate import count, xs

    truth = torch.tensor([0.75])
    data = (
        dict(obs=xs.double())
        if par
        else {f"obs_{i}": x for i, x in enumerate(xs.double())}
    )

    conditioned_model = pyro.condition(model, data=data)
    conditioned_model(count)

    importance = Importance(conditioned_model, guide=None, num_samples=args.num_samples)

    print("running importance sampling...")
    start = time.time()
    emp_marginal = EmpiricalMarginal(importance.run(count), sites=["p"])
    end = time.time()

    posterior_marg = emp_marginal.mean.detach()
    lws = torch.vstack(importance.log_weights)

    torch.set_printoptions(sci_mode=False, precision=3, linewidth=120)
    print("  estimates:", posterior_marg)
    print("      truth:", truth)
    print("         L1: {:.3f}".format((posterior_marg - truth).abs().sum().item()))
    print(
        "        ESS: {:.3f} / {}".format(importance.get_ESS().item(), args.num_samples)
    )
    print(
        "Min/Max lw: {:.3f} <= log(w) <= {:.3f}".format(
            lws.min().item(), lws.max().item()
        )
    )
    print("  E[log(w)]: {:.3f}".format(lws.mean().item()))
    print("Var[log(w)]: {:.3f}".format(lws.std().item()))
    print("-----------------------------------------")
    print("  wallclock: {:.3f}s".format(end - start))
    print()
