import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import torch
import pyro
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal
from utils import *

def network(suffix=""):
    def flip(n, p):
        return pyro.sample(n+suffix, dist.Bernoulli(p))
    n50r = True
    n40r = flip("n40r", 1.0 / 2.0) if     n50r else torch.zeros(1)
    n51r = flip("n51r", 1.0 / 2.0) if not n40r else torch.zeros(1)
    n30r = flip("n30r", 1.0 / 2.0) if     n40r else torch.zeros(1)
    n41r = flip("n41r", 1.0 / 2.0) if not n30r else torch.zeros(1)
    n20r = flip("n20r", 1.0 / 2.0) if     n30r else torch.zeros(1)
    n31r = flip("n31r", 1.0 / 2.0) if not n20r else torch.zeros(1)
    n42r = flip("n42r", 1.0 / 2.0) if     n31r else torch.zeros(1)
    n43r = flip("n43r", 1.0 / 2.0) if not n31r else torch.zeros(1)
    n10r = flip("n10r", 1.0 / 2.0) if     n20r else torch.zeros(1)
    n21r = flip("n21r", 1.0 / 2.0) if not n10r else torch.zeros(1)
    n32r = flip("n32r", 1.0 / 2.0) if     n21r else torch.zeros(1)
    n33r = flip("n33r", 1.0 / 2.0) if not n21r else torch.zeros(1)
    n44r = flip("n44r", 1.0 / 2.0) if     n32r else torch.zeros(1)
    n45r = flip("n45r", 1.0 / 2.0) if not n32r else torch.zeros(1)
    n46r = flip("n46r", 1.0 / 2.0) if     n33r else torch.zeros(1)
    n47r = flip("n47r", 1.0 / 2.0) if not n33r else torch.zeros(1)
    n52r = flip("n52r", 1.0 / 2.0) if     n41r else torch.zeros(1)
    n53r = flip("n53r", 1.0 / 2.0) if not n41r else torch.zeros(1)
    n54r = flip("n54r", 1.0 / 2.0) if     n42r else torch.zeros(1)
    n55r = flip("n55r", 1.0 / 2.0) if not n42r else torch.zeros(1)
    n56r = flip("n56r", 1.0 / 2.0) if     n43r else torch.zeros(1)
    n57r = flip("n57r", 1.0 / 2.0) if not n43r else torch.zeros(1)
    n58r = flip("n58r", 1.0 / 2.0) if     n44r else torch.zeros(1)
    n59r = flip("n59r", 1.0 / 2.0) if not n44r else torch.zeros(1)
    n510r = flip("n510r", 1.0 / 2.0) if     n45r else torch.zeros(1)
    n511r = flip("n511r", 1.0 / 2.0) if not n45r else torch.zeros(1)
    n512r = flip("n512r", 1.0 / 2.0) if     n46r else torch.zeros(1)
    n513r = flip("n513r", 1.0 / 2.0) if not n46r else torch.zeros(1)
    n514r = flip("n514r", 1.0 / 2.0) if     n47r else torch.zeros(1)
    n515r = flip("n515r", 1.0 / 2.0) if not n47r else torch.zeros(1)
    n0   = n10r
    n10l = flip("n10l", 1.0 / 2.0) if    n0   else torch.zeros(1)
    n20l = flip("n20l", 1.0 / 2.0) if     n10l else torch.zeros(1)
    n21l = flip("n21l", 1.0 / 2.0) if not n10l else torch.zeros(1)
    n30l = flip("n30l", 1.0 / 2.0) if     n20l else torch.zeros(1)
    n31l = flip("n31l", 1.0 / 2.0) if not n20l else torch.zeros(1)
    n32l = flip("n32l", 1.0 / 2.0) if     n21l else torch.zeros(1)
    n33l = flip("n33l", 1.0 / 2.0) if not n21l else torch.zeros(1)
    n40l = flip("n40l", 1.0 / 2.0) if     n30l else torch.zeros(1)
    n41l = flip("n41l", 1.0 / 2.0) if not n30l else torch.zeros(1)
    n42l = flip("n42l", 1.0 / 2.0) if     n31l else torch.zeros(1)
    n43l = flip("n43l", 1.0 / 2.0) if not n31l else torch.zeros(1)
    n44l = flip("n44l", 1.0 / 2.0) if     n32l else torch.zeros(1)
    n45l = flip("n45l", 1.0 / 2.0) if not n32l else torch.zeros(1)
    n46l = flip("n46l", 1.0 / 2.0) if     n33l else torch.zeros(1)
    n47l = flip("n47l", 1.0 / 2.0) if not n33l else torch.zeros(1)
    n50l = flip("n50l", 1.0 / 2.0) if     n40l else torch.zeros(1)
    n51l = flip("n51l", 1.0 / 2.0) if not n40l else torch.zeros(1)
    n52l = flip("n52l", 1.0 / 2.0) if     n41l else torch.zeros(1)
    n53l = flip("n53l", 1.0 / 2.0) if not n41l else torch.zeros(1)
    n54l = flip("n54l", 1.0 / 2.0) if     n42l else torch.zeros(1)
    n55l = flip("n55l", 1.0 / 2.0) if not n42l else torch.zeros(1)
    n56l = flip("n56l", 1.0 / 2.0) if     n43l else torch.zeros(1)
    n57l = flip("n57l", 1.0 / 2.0) if not n43l else torch.zeros(1)
    n58l = flip("n58l", 1.0 / 2.0) if     n44l else torch.zeros(1)
    n59l = flip("n59l", 1.0 / 2.0) if not n44l else torch.zeros(1)
    n510l = flip("n510l", 1.0 / 2.0) if     n45l else torch.zeros(1)
    n511l = flip("n511l", 1.0 / 2.0) if not n45l else torch.zeros(1)
    n512l = flip("n512l", 1.0 / 2.0) if     n46l else torch.zeros(1)
    n513l = flip("n513l", 1.0 / 2.0) if not n46l else torch.zeros(1)
    n514l = flip("n514l", 1.0 / 2.0) if     n47l else torch.zeros(1)
    n515l = flip("n515l", 1.0 / 2.0) if not n47l else torch.zeros(1)

    return (n50l ,  n51l ,  n52l ,  n53l ,  n54l ,  n55l ,  n56l ,  n57l ,  n58l ,  n59l ,  n510l ,  n511l ,  n512l ,  n513l ,  n514l ,  n515l)

sites = ["npackets"]
truth = [
( 2 * 0.0         - 2 * (1 - 0.0)
+ 2 * 0.5         - 2 * (1 - 0.5)
+ 2 * 0.249019608 - 2 * (1 - 0.249019608)
+ 2 * 0.250980392 - 2 * (1 - 0.250980392)
+ 2 * 0.123529412 - 2 * (1 - 0.123529412)
+ 2 * 0.376470588 - 2 * (1 - 0.376470588)
+ 2 * 0.126470588 - 2 * (1 - 0.126470588)
+ 2 * 0.373529412 - 2 * (1 - 0.373529412)
+ 2 * 0.060784314 - 2 * (1 - 0.060784314)
+ 2 * 0.439215686 - 2 * (1 - 0.439215686)
+ 2 * 0.189215686 - 2 * (1 - 0.189215686)
+ 2 * 0.310784314 - 2 * (1 - 0.310784314)
+ 2 * 0.064215686 - 2 * (1 - 0.064215686)
+ 2 * 0.435784314 - 2 * (1 - 0.435784314)
+ 2 * 0.185784314 - 2 * (1 - 0.185784314)
+ 2 * 0.314215686 - 2 * (1 - 0.314215686)
 ) ]
print(truth)

def model():
    npackets = pyro.sample("npackets", dist.Poisson(3))
    expt = 0.0
    for ix in pyro.plate("packet", int(npackets.item())):
        netfwd = pyro.condition(network, data={f"n40l_{ix}": torch.zeros(1)})
        ns = netfwd(suffix=f"_{ix}")
        expt += pyro.sample(f"l0_{ix}", dist.Normal((2.0 if ns[0].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l1_{ix}", dist.Normal((2.0 if ns[1].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l2_{ix}", dist.Normal((2.0 if ns[2].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l3_{ix}", dist.Normal((2.0 if ns[3].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l4_{ix}", dist.Normal((2.0 if ns[4].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l5_{ix}", dist.Normal((2.0 if ns[5].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l6_{ix}", dist.Normal((2.0 if ns[6].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l7_{ix}", dist.Normal((2.0 if ns[7].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l8_{ix}", dist.Normal((2.0 if ns[8].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l9_{ix}", dist.Normal((2.0 if ns[9].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l10_{ix}", dist.Normal((2.0 if ns[10].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l11_{ix}", dist.Normal((2.0 if ns[11].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l12_{ix}", dist.Normal((2.0 if ns[12].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l13_{ix}", dist.Normal((2.0 if ns[13].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l14_{ix}", dist.Normal((2.0 if ns[14].item() else -2.0) * torch.ones(1), torch.ones(1)))
        expt += pyro.sample(f"l15_{ix}", dist.Normal((2.0 if ns[15].item() else -2.0) * torch.ones(1), torch.ones(1)))
    return expt

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="generate data for simple HMMs")
    parser.add_argument("--num-samples", default=1_000, type=int,)
    parser.add_argument("--num-runs", default=1, type=int,)
    parser.add_argument("--seed", default=0, type=int,)
    args = parser.parse_args()

    if args.num_runs > 1:
        (l1s, times) = runall(model, sites, truth, num_runs=args.num_runs, num_samples=args.num_samples, start_seed=args.seed)
        print("--------")
        runs = len(l1s)
        print(f"averages over {runs} runs:")
        print("wallclock:", sum(times) / len(times), "s")
        print("       L1:", sum(l1s) / len(l1s))
    else:
        # we are benchmarking, expect the same output as yodel
        torch.manual_seed(args.seed)
        np.random.seed(args.seed)
        random.seed(args.seed)
        start = time.time()
        importance = Importance(model, num_samples=args.num_samples)
        posterior = importance.run()
        xs = [torch.tensor([tr.nodes["_RETURN"]["value"] for tr in importance.exec_traces]).mean()]
        end = time.time()
        s = end - start
        print(" ".join([f"{x}" for x in xs]))
        print("{:.3f}ms".format(s * 1000))
