use crate::*;
use itertools::Itertools;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::builder::bdd_plan::BddPlan;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::*;
use tracing::*;

pub fn calculate_wmc_prob_h(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
    var_order: &VarOrder,
    dist: BddPlan,
    accept: BddPlan,
) -> (f64, f64, WmcStats) {
    let dist = mgr.compile_plan(&dist);
    let accept = mgr.compile_plan(&accept);
    let num = mgr.and(dist, accept);
    debug!("{:?} /\\ {:?} = {:?}", dist, accept, num);
    debug!("-------------");
    debug!("{:?}", accept);

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
    dist: BddPlan,
    accept: BddPlan,
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
    dist: BddPlan,
    accept: BddPlan,
    samples: BddPlan,
) -> (f64, WmcStats) {
    let span = tracing::span!(tracing::Level::DEBUG, "calculate_wmc_prob");
    let _enter = span.enter();
    let accept = BddPlan::and(samples, accept);
    calculate_wmc_prob_hf64(mgr, params, var_order, dist, accept)
}

pub fn wmc_prob(mgr: &mut Mgr, c: &EOutput) -> (Vec<f64>, Option<WmcStats>) {
    let span = tracing::span!(tracing::Level::DEBUG, "wmc_prob");
    let _enter = span.enter();
    let mut last_stats = None;
    let probs = c
        .dists()
        .iter()
        .map(|d| {
            let (p, stats) = calculate_wmc_prob(
                mgr,
                &c.weightmap.as_params(mgr.get_order().num_vars() as u64),
                &mgr.get_order().clone(),
                d.clone(),
                c.accept.clone(),
                c.samples(),
            );
            last_stats = Some(stats);
            p
        })
        .collect_vec();
    (probs, last_stats)
}
