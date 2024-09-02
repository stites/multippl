use crate::data::HashMap;
use crate::*;
use rand::distributions::Distribution;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::repr::wmc::RealSemiring;
use std::f64;
use tracing::*;

#[inline(always)]
pub fn sample_from<D: Distribution<V>, V>(state: &mut super::eval::State, dist: D) -> V {
    match state.rng.as_mut() {
        Some(rng) => dist.sample(rng),
        None => dist.sample(&mut rand::thread_rng()),
    }
}
#[inline(always)]
fn mk_output_samples(mgr: &mut Mgr, prev_samples: &BddPtr, dist: BddPtr, sample: bool) -> BddPtr {
    let new_sample = mgr.iff(dist, BddPtr::from_bool(sample));
    mgr.and(*prev_samples, new_sample)
}

pub fn magsafe(theta_q: f64) -> f64 {
    let tolerance = std::f64::EPSILON * 10.0; // there might be some math on these floats, maybe one order of magnitude more than eps
    if (1.0 - theta_q).abs() < tolerance {
        1.0
    } else {
        if theta_q.abs() < tolerance {
            0.0
        } else {
            theta_q
        }
    }
}
#[inline]
pub fn exact2sample_bdd_eff(
    state: &mut super::eval::State,
    out: &mut Output,
    dist: &BddPtr,
) -> bool {
    // let wmc_params = out.exact.weightmap.as_params(state.opts.max_label);
    let wmc_params = state.wmc.params();
    let accept = out.exact.accept;
    let ss = GetSamples::samples(&out.exact, state.mgr, state.opts.sample_pruning);
    debug!(" samples: {:?}", out.exact.samples);
    debug!("csamples: {:?}", ss);
    debug!("  dist size: {}", dist.count_nodes());
    debug!("accept size: {}", accept.count_nodes());
    let theta_q =
        magsafe(crate::inference::calculate_wmc_prob(state.mgr, wmc_params, *dist, accept, ss).0);
    debug!("    theta_q: {}", theta_q);
    debug!(" #rec calls: {}", state.mgr.num_recursive_calls());

    // for v in crate::utils::variables(*dist) {
    //     debug!("w {:?}: {:?}", v, wmc_params.get_var_weight(v));
    // }

    let bern = statrs::distribution::Bernoulli::new(theta_q).unwrap();
    let s = sample_from(state, bern) == 1.0;

    let weight = if s { theta_q } else { 1.0 - theta_q };
    // state.mult_pq(weight, weight);

    // sample in sequence. A smarter sample would compile
    // all samples of a multi-rooted BDD, but I need to futz
    // with rsdd's fold
    let new_samples = mk_output_samples(state.mgr, &out.exact.samples, *dist, s);
    out.exact.samples = new_samples;
    state.boundaries_seen += 1;
    s
}
