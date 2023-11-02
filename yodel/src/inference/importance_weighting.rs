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
    let mut e = Exp2::empty();

    debug!("running with options: {:#?}", opt);
    for step in 1..=steps {
        if step % 100 == 1 {
            debug!("step: {step}");
        }
        match crate::runner_with_data(&mut mgr, &mut rng, &opt, &p, &lenv, step, &dview) {
            Ok(o) => {
                trace!("{:?}", o.out.exact.out);
                let (out, pq) = (o.out, o.pq);
                debug!("sample output : {:?}", out.sample.out);
                debug!("sample trace  : {:?}", out.sample.trace);
                debug!("exact output  : {:?}", out.exact.out);
                debug!("accepting     : {:?}", out.exact.accept);
                let (query, ws): (Vec<f64>, Vec<f64>) = match p.query() {
                    // TODO drop this traversal, pre-compute during eval
                    Query::EQuery(_) => {
                        let nums = numerators(&mut mgr, &out.exact, true);
                        debug!("          nums: {:?}", nums);
                        let var_order = mgr.get_order().clone();
                        let params = out
                            .exact
                            .weightmap
                            .as_params(mgr.get_order().num_vars() as u64);

                        let final_samples = out.exact.samples(&mut mgr);
                        let final_accept = mgr.and(final_samples, out.exact.accept);
                        let RealSemiring(denom) = final_accept.wmc(&var_order, &params);

                        let RealSemiring(wmc_accept) = out.exact.accept.wmc(&var_order, &params);
                        debug!("    wmc accept: {:?}", wmc_accept);
                        // let qs = queries(&mut mgr, &out.exact);
                        // debug!("            qs: {:?}", qs);
                        // let ws = izip!(qs, &nums).map(|(q, num)| {
                        //     if q == 0.0 { 0.0 } else { *num / q }
                        // }).collect_vec();
                        // debug!("      num / qs: {:?}", ws);
                        // let ws = ws.into_iter().map(|w| {
                        //     w / wmc_accept
                        // }).collect_vec();
                        // debug!("    w / accept: {:?}", ws);

                        // let's just stick to what I /thought/ should work:
                        let ws = nums
                            .iter()
                            .map(|_| {
                                // if wmc_accept == 0.0 { 0.0 } else { wmc_accept / denom }
                                // when we observe we _do_ want to weight by the
                                // accepting criteria, even if we already compute it
                                // in the query. Why is this not double-counting?
                                // if wmc_accept == 0.0 { 0.0 } else { wmc_accept }
                                if denom == 0.0 {
                                    0.0
                                } else {
                                    denom
                                }
                            })
                            .collect_vec();
                        let full_wmc = wmc_prob(&mut mgr, &out.exact).0;

                        // (full_wmc, ws)
                        (nums, ws)
                    }
                    Query::SQuery(_) => {
                        // FIXME: ensure that you're not missing something about incorporating the accepting criteria into the weight!
                        let qs = out.sample.out.iter().fold(vec![], |mut acc, v| {
                            let vs: Vec<f64> = From::<&SVal>::from(v);
                            acc.extend(vs);
                            acc
                        });
                        debug!("         query: {:?}", qs);
                        debug!("weight        : {} = {}", pq.render(), pq.weight());
                        let ws = qs.iter().map(|_| pq.weight()).collect_vec();
                        (qs, ws)
                    }
                };
                e.add(Exp2::new(ws, query));
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
