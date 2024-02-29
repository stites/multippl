use crate::*;
use rand::distributions::Distribution;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::repr::wmc::RealSemiring;
use crate::data::HashMap;
use tracing::*;

pub fn sample_from<D: Distribution<V>, V>(state: &mut super::eval::State, dist: D) -> V {
    match state.rng.as_mut() {
        Some(rng) => dist.sample(rng),
        None => dist.sample(&mut rand::thread_rng()),
    }
}
#[cfg(feature = "debug_samples")]
fn mk_output_samples(
    mgr: &mut Mgr,
    prev_samples: &HashMap<BddPtr, bool>,
    dist: BddPtr,
    sample: bool,
) -> HashMap<BddPtr, bool> {
    let mut samples = prev_samples.clone();
    samples.insert(dist.clone(), sample);
    samples
}
#[cfg(not(feature = "debug_samples"))]
fn mk_output_samples(mgr: &mut Mgr, prev_samples: &BddPtr, dist: BddPtr, sample: bool) -> BddPtr {
    let new_sample = mgr.iff(dist, BddPtr::from_bool(sample));
    mgr.and(*prev_samples, new_sample)
}
pub fn exact2sample_bdd_eff(
    state: &mut super::eval::State,
    out: &mut Output,
    dist: &BddPtr,
) -> bool {
    let wmc_params = out.exact.weightmap.as_params(state.opts.max_label);
    let var_order = state.opts.order.clone();
    let accept = out.exact.accept;
    let ss = GetSamples::samples(&out.exact, state.mgr, state.opts.sample_pruning);
    debug!(" samples: {:?}", out.exact.samples);
    debug!("csamples: {:?}", ss);
    debug!("  dist size: {}", dist.count_nodes());
    debug!("accept size: {}", accept.count_nodes());
    let theta_q =
        crate::inference::calculate_wmc_prob(state.mgr, &wmc_params, &var_order, *dist, accept, ss)
            .0;
    debug!(" #rec calls: {}", state.mgr.num_recursive_calls());

    let bern = statrs::distribution::Bernoulli::new(theta_q).unwrap();
    let s = sample_from(state, bern) == 1.0;

    let weight = if s { theta_q } else { 1.0 - theta_q };
    // state.mult_pq(weight, weight);

    out.sample
        .trace
        .push((SVal::SBool(s), Dist::Bern(theta_q), weight, None));

    // sample in sequence. A smarter sample would compile
    // all samples of a multi-rooted BDD, but I need to futz
    // with rsdd's fold
    let new_samples = mk_output_samples(state.mgr, &out.exact.samples, *dist, s);
    out.exact.samples = new_samples;
    s
}
