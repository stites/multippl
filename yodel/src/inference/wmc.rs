use crate::*;
use itertools::Itertools;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::*;
use tracing::*;

pub fn calculate_wmc_prob_h(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
) -> (f64, f64, WmcStats) {
    let num = mgr.and(dist, accept);
    debug!(
        "{} /\\ {} = {}",
        dist.print_bdd(),
        accept.print_bdd(),
        num.print_bdd()
    );
    debug!("-------------");
    debug!("{}", accept.print_bdd());

    let RealSemiring(a) = num.wmc(var_order, params);
    let RealSemiring(z) = accept.wmc(var_order, params);
    debug!("{}", a);
    debug!("----------- = {}", a / z);
    debug!("{}", z);
    (
        a,
        z,
        WmcStats {
            dist: dist.count_nodes(),
            dist_accept: num.count_nodes(),
            accept: accept.count_nodes(),
            mgr_recursive_calls: mgr.num_recursive_calls(),
        },
    )
}

pub fn calculate_wmc_prob_hf64(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
) -> (f64, WmcStats) {
    let (a, z, stats) = calculate_wmc_prob_h(mgr, params, var_order, dist, accept);
    if a == 0.0 {
        (0.0, stats)
    } else {
        (a / z, stats)
    }
}

pub fn calculate_wmc_prob(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
    samples: BddPtr,
) -> (f64, WmcStats) {
    let span = tracing::span!(tracing::Level::DEBUG, "calculate_wmc_prob");
    let _enter = span.enter();
    let accept = mgr.and(samples, accept);
    calculate_wmc_prob_hf64(mgr, params, var_order, dist, accept)
}

pub fn wmc_prob(mgr: &mut Mgr, c: &Output) -> (Vec<f64>, WmcStats) {
    let span = tracing::span!(tracing::Level::DEBUG, "wmc_prob");
    let _enter = span.enter();
    let mut last_stats = None;
    let probs = c
        .dists
        .iter()
        .map(|d| {
            let (p, stats) = calculate_wmc_prob(
                mgr,
                &c.weightmap.as_params(mgr.get_order().num_vars() as u64),
                &mgr.get_order().clone(),
                *d,
                c.accept,
                c.samples,
            );
            last_stats = Some(stats);
            p
        })
        .collect_vec();
    (probs, last_stats.expect("output dists should be non-empty"))
}
