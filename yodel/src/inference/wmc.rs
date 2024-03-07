use crate::*;
use itertools::Itertools;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::*;
use tracing::*;

#[inline(always)]
pub fn calculate_wmc_prob_h(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
    dist: BddPtr,
    accept: BddPtr,
) -> (f64, f64, Option<WmcStats>) {
    // let dist = mgr.compile_plan(&dist);
    // let accept = mgr.compile_plan(&accept);
    let num = mgr.and(dist, accept);
    debug!("{:?} /\\ {:?} = {:?}", dist, accept, num);
    debug!("-------------");
    debug!("{:?}", accept);
    let var_order = mgr.get_order();

    let RealSemiring(a) = num.wmc(var_order, params);
    let RealSemiring(z) = accept.wmc(var_order, params);
    debug!("{}", a);
    debug!("----------- = {}", a / z);
    debug!("{}", z);
    (
        a, z,
        None,
        // Some(WmcStats {
        //     dist: dist.count_nodes(),
        //     dist_accept: num.count_nodes(),
        //     accept: accept.count_nodes(),
        //     mgr_recursive_calls: mgr.num_recursive_calls(),
        // }),
    )
}

#[inline(always)]
pub fn calculate_wmc_prob_hf64(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
    dist: BddPtr,
    accept: BddPtr,
) -> (f64, Option<WmcStats>) {
    let (a, z, stats) = calculate_wmc_prob_h(mgr, params, dist, accept);
    if a == 0.0 {
        (0.0, stats)
    } else {
        (a / z, stats)
    }
}

#[inline(always)]
pub fn calculate_wmc_prob(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
    dist: BddPtr,
    accept: BddPtr,
    samples: BddPtr,
) -> (f64, Option<WmcStats>) {
    let span = tracing::span!(tracing::Level::DEBUG, "calculate_wmc_prob");
    let _enter = span.enter();
    let accept = mgr.and(samples, accept);
    calculate_wmc_prob_hf64(mgr, params, dist, accept)
}

pub fn wmc_prob(mgr: &mut Mgr, wmc: &WmcP, c: &EOutput) -> (Vec<f64>, Option<WmcStats>) {
    let span = tracing::span!(tracing::Level::DEBUG, "wmc_prob");
    let _enter = span.enter();
    let mut last_stats = None;
    let probs = c
        .dists()
        .iter()
        .map(|d| {
            let ss = c.samples(mgr);
            let (p, stats) = calculate_wmc_prob(
                mgr,
                // &c.weightmap.as_params(mgr.get_order().num_vars() as u64),
                wmc.params(),
                *d,
                c.accept,
                ss,
            );
            last_stats = stats;
            p
        })
        .collect_vec();
    (probs, last_stats)
}

pub fn numerators(mgr: &mut Mgr, wmc: WmcP, c: &EOutput, fold_in_samples: bool) -> Vec<f64> {
    let span = tracing::span!(tracing::Level::DEBUG, "numerators");
    let _enter = span.enter();

    let accept = if fold_in_samples {
        let ss = c.samples(mgr);
        mgr.and(ss, c.accept)
    } else {
        c.accept
    };
    // let params = c.weightmap.as_params(mgr.get_order().num_vars() as u64);
    let params = wmc.params();

    let probs = c
        .dists()
        .iter()
        .map(|dist| {
            let numerator_formula = mgr.and(*dist, accept);
            let RealSemiring(num) = numerator_formula.wmc(mgr.get_order(), params);
            num
        })
        .collect_vec();
    probs
}

pub fn queries(mgr: &mut Mgr, w: &WmcP, c: &EOutput) -> Vec<f64> {
    let span = tracing::span!(tracing::Level::DEBUG, "queries");
    let _enter = span.enter();
    // let params = c.weightmap.as_params(mgr.get_order().num_vars() as u64);
    let params = w.params();

    let probs = c
        .dists()
        .iter()
        .map(|dist| {
            let RealSemiring(num) = dist.wmc(mgr.get_order(), params);
            num
        })
        .collect_vec();
    probs
}
