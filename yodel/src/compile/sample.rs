use crate::*;
use rand::distributions::Distribution;
use tracing::*;

pub fn sample_from<D: Distribution<V>, V>(state: &mut super::eval::State, dist: D) -> V {
    match state.rng.as_mut() {
        Some(rng) => dist.sample(rng),
        None => dist.sample(&mut rand::thread_rng()),
    }
}

pub fn exact2sample_bdd_eff(
    state: &mut super::eval::State,
    out: &mut Output,
    dist: &BddPtr,
) -> bool {
    let wmc_params = out.exact.weightmap.as_params(state.opts.max_label);
    let var_order = state.opts.order.clone();
    let accept = out.exact.accept.clone();
    info!("accepting: {}", accept.print_bdd());
    let ss = GetSamples::samples(&out.exact, state.mgr, state.opts.sample_pruning);
    info!("samples: {:?}", out.exact.samples); // we're getting a state explosion here
    let theta_q = crate::inference::calculate_wmc_prob(
        state.mgr,
        &wmc_params,
        &var_order,
        dist.clone(),
        accept.clone(),
        ss,
    )
    .0;

    let bern = statrs::distribution::Bernoulli::new(theta_q).unwrap();
    let s = sample_from(state, bern) == 1.0;

    let weight = if s { theta_q } else { 1.0 - theta_q };
    state.mult_pq(weight, weight);

    out.sample.trace.push((
        SVal::SBool(s),
        Dist::Bern(theta_q),
        Probability::new(weight),
        None,
    ));

    // sample in sequence. A smarter sample would compile
    // all samples of a multi-rooted BDD, but I need to futz
    // with rsdd's fold
    out.exact.samples.insert(dist.clone(), s);
    s
}
