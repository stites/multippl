use crate::inference::debug::*;
use crate::inference::wmc::*;
use crate::uniquify::grammar::*;
use crate::utils::render::*;
use crate::*;
use itertools::*;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::repr::wmc::RealSemiring;
use std::collections::HashMap;
use tracing::*;

pub fn importance_weighting(steps: usize, p: &str) -> Vec<f64> {
    importance_weighting_h(
        steps,
        p,
        &crate::Options {
            opt: false,
            ..Default::default()
        },
    )
    .0
}
pub fn importance_weighting_h(
    steps: usize,
    code: &str,
    opt: &crate::Options,
) -> (Vec<f64>, Option<WmcStats>) {
    importance_weighting_h_h(steps, code, opt, Default::default())
}

pub fn importance_weighting_h_h(
    steps: usize,
    code: &str,
    opt: &crate::Options,
    data: DataPoints,
) -> (Vec<f64>, Option<WmcStats>) {
    let ds = DataSet::new(data);
    let (mut mgr, p, lenv, dview) = make_mgr_and_ir_with_data(code, ds).unwrap();
    let mut rng = opt.rng();
    let mut e = Exp1::empty();

    debug!("running with options: {:#?}", opt);
    for step in 1..=steps {
        if step % 100 == 1 {
            debug!("step: {step}");
        }
        let step0ix = step - 1; // fix off-by-one for data view
        match crate::runner_with_data(&mut mgr, &mut rng, opt, &p, &lenv, step0ix, &dview) {
            Ok(o) => {
                let (out, pq) = (o.out, o.pq);
                trace!("sample output : {:?}", out.sample.out);
                trace!("sample trace  : {:?}", out.sample.trace);
                trace!("exact output  : {:?}", out.exact.out);
                trace!("accepting     : {:?}", out.exact.accept);
                let var_order = mgr.get_order().clone();
                let params = out
                    .exact
                    .weightmap
                    .as_params(mgr.get_order().num_vars() as u64);

                let samples = out.exact.samples(&mut mgr);
                let accept = out.exact.accept;
                let final_accept = mgr.and(samples, accept);
                let RealSemiring(wmc_accept) = accept.wmc(&var_order, &params);
                let RealSemiring(wmc_final_accept) = final_accept.wmc(&var_order, &params);
                let RealSemiring(wmc_sample) = samples.wmc(&var_order, &params);

                let lwmc_accept = LW::new(wmc_accept);
                let lwmc_final_accept = LW::new(wmc_final_accept);
                let lwmc_sample = LW::new(wmc_sample);

                let (lquery, lw): (Vec<LW>, LW) = match p.query() {
                    // TODO drop this traversal, pre-compute during eval
                    Query::EQuery(_) => {
                        // the final accepting criteria is a & s. Normalize the query.
                        let lquery = out
                            .exact
                            .dists()
                            .iter()
                            .map(|d| {
                                if lwmc_accept == LW(std::f64::NEG_INFINITY) {
                                    LW(std::f64::NEG_INFINITY)
                                } else {
                                    let num = mgr.and(*d, final_accept);
                                    let RealSemiring(a) = num.wmc(&var_order, &params);
                                    LW::new(a).sub(lwmc_final_accept)
                                }
                            })
                            .collect_vec();

                        // Weight the query by ratio of a & s : s
                        let w = LW::new(wmc_final_accept).sub(LW::new(wmc_sample));

                        (lquery, w)
                    }
                    Query::SQuery(_) => {
                        // FIXME: ensure that you're not missing something about incorporating the accepting criteria into the weight!
                        let qs = out
                            .sample
                            .out
                            .iter()
                            .fold(vec![], |mut acc, v| {
                                let vs: Vec<f64> = From::<&SVal>::from(v);
                                acc.extend(vs);
                                acc
                            })
                            .into_iter()
                            // .map(|x| LW::new(x))
                            .map(LW::new)
                            .collect_vec();
                        // let ws = qs.iter().map(|_| pq.weight()).collect_vec();
                        let w = LW::new(wmc_final_accept).sub(LW::new(wmc_sample));
                        let final_weight = pq.log_weight().add(w);
                        trace!("    final_weight: {}", final_weight.log_render());
                        trace!("           query: {:?}", qs);
                        trace!("-----------------------");

                        (qs, final_weight)
                    }
                };
                e.add(Exp1::new(lw, lquery));
            }
            Err(e) => panic!(
                "Error type: {}{}",
                e.errtype(),
                e.msg()
                    .map(|x| format!("\nMessage: {}", x))
                    .unwrap_or_else(|| "".to_string())
            ),
        }
    }

    // let exp = e.exp.clone();
    // let expw = e.expw.clone();
    debug!("∑[w * f(-)]: {:?}", e.wquery_sums);
    debug!("       ∑[w]: {:?}", e.sum_w);

    // // debug_importance_weighting(true, steps, &ws, &[], &prs, &sss, &exp, &expw);

    // let x = e.query();
    // info!("E[q]: {}", renderfloats(&x, false));
    // (x, stats_max)
    (e.query(), None)
}
