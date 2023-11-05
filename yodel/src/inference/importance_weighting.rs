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
        match crate::runner_with_data(&mut mgr, &mut rng, &opt, &p, &lenv, step, &dview) {
            Ok(o) => {
                trace!("{:?}", o.out.exact.out);
                let (out, pq) = (o.out, o.pq);
                debug!("sample output : {:?}", out.sample.out);
                debug!("sample trace  : {:?}", out.sample.trace);
                debug!("exact output  : {:?}", out.exact.out);
                debug!("accepting     : {:?}", out.exact.accept);
                let (query, ws): (Vec<f64>, f64) = match p.query() {
                    // TODO drop this traversal, pre-compute during eval
                    Query::EQuery(_) => {
                        let var_order = mgr.get_order().clone();
                        let params = out
                            .exact
                            .weightmap
                            .as_params(mgr.get_order().num_vars() as u64);

                        let final_samples = out.exact.samples(&mut mgr);
                        let final_accept = mgr.and(final_samples, out.exact.accept);
                        let RealSemiring(accept_samples) = final_accept.wmc(&var_order, &params);

                        let RealSemiring(wmc_accept) = out.exact.accept.wmc(&var_order, &params);
                        let RealSemiring(wmc_sample) = out.exact.samples.wmc(&var_order, &params);
                        let wmc_nums = numerators(&mut mgr, &out.exact, false);
                        let wmc_nums_ss = numerators(&mut mgr, &out.exact, true);
                        // let qs = queries(&mut mgr, &out.exact);
                        // println!("            qs: {:?}", qs);
                        // let ws = izip!(qs, &nums).map(|(q, num)| {
                        //     if q == 0.0 { 0.0 } else { *num / q }
                        // }).collect_vec();
                        // println!("      num / qs: {:?}", ws);
                        // let ws = ws.into_iter().map(|w| {
                        //     w / wmc_accept
                        // }).collect_vec();
                        // println!("    w / accept: {:?}", ws);


                        // let full_wmc = wmc_prob(&mut mgr, &out.exact).0;
                        let wmc_numa_dena = out.exact
                            .dists()
                            .iter()
                            .map(|d| {
                                let nvs = mgr.get_order().num_vars() as u64;
                                let o = mgr.get_order().clone();
                                let accept = out.exact.accept.clone();

                                let num = mgr.and(*d, accept);
                                let RealSemiring(a) = num.wmc(&o, &params);

                                let denom = accept;
                                let RealSemiring(z) = denom.wmc(&o, &params);
                                if z == 0.0 { 0.0 } else { a / z }
                            })
                            .collect_vec();
                        let wmc_numa_denas = out.exact
                            .dists()
                            .iter()
                            .map(|d| {
                                let nvs = mgr.get_order().num_vars() as u64;
                                let o = mgr.get_order().clone();
                                let samples = out.exact.samples(&mut mgr);
                                let accept = out.exact.accept.clone();

                                let num = mgr.and(*d, accept);
                                let RealSemiring(a) = num.wmc(&o, &params);

                                let denom = accept;
                                let denom = mgr.and(denom, samples);
                                let RealSemiring(z) = denom.wmc(&o, &params);
                                if z == 0.0 { 0.0 } else { a / z }
                            })
                            .collect_vec();
                        let wmc_numas_dena = out.exact
                            .dists()
                            .iter()
                            .map(|d| {
                                let nvs = mgr.get_order().num_vars() as u64;
                                let o = mgr.get_order().clone();
                                let samples = out.exact.samples(&mut mgr);
                                let accept = out.exact.accept.clone();

                                let num = mgr.and(*d, accept);
                                let num = mgr.and(num, samples);
                                let RealSemiring(a) = num.wmc(&o, &params);

                                let denom = accept;
                                let RealSemiring(z) = denom.wmc(&o, &params);
                                if z == 0.0 { 0.0 } else { a / z }
                            })
                            .collect_vec();
                        let wmc_numas_denas = out.exact
                            .dists()
                            .iter()
                            .map(|d| {
                                let nvs = mgr.get_order().num_vars() as u64;
                                let o = mgr.get_order().clone();
                                let samples = out.exact.samples(&mut mgr);
                                let accept = out.exact.accept.clone();

                                let num = mgr.and(*d, accept);
                                let num = mgr.and(num, samples);
                                let RealSemiring(a) = num.wmc(&o, &params);

                                let denom = accept;
                                let denom = mgr.and(denom, samples);
                                let RealSemiring(z) = denom.wmc(&o, &params);
                                if z == 0.0 { 0.0 } else { a / z }
                            })
                            .collect_vec();
                        // println!("      final ws: {:?}", ws);
                        println!("num:");
                        println!("     wmc d&a  : {:?}", wmc_nums);
                        println!("     wmc d&a&s: {:?}", wmc_nums_ss);
                        println!("denom:");
                        println!("    wmc accept: {:?}", wmc_accept);
                        println!("   wmc samples: {:?}", wmc_sample);
                        println!("   wmc a && ss: {:?}", accept_samples);
                        println!("full_wmc:");
                        println!("  phi&a   / a : {:?}", wmc_numa_dena);
                        println!("  phi&a   / as: {:?}", wmc_numa_denas);
                        println!("  phi&a&s / a : {:?}", wmc_numas_dena);
                        println!("  phi&a&s / as: {:?}", wmc_numas_denas);
                        let w =
                        // let ws = wmc_nums
                        //     .iter()
                        //     .map(|_| {
                                // // proposal 1: fixes sample consistency, fails to turn exact-only programs into samplers
                                // if accept_samples == 0.0 { 0.0 } else {
                                //      wmc_sample  / accept_samples
                                // }
                                // // proposal 2: should fix problems in ^^^, breaks sample-consistency
                                // if wmc_accept == 0.0 { 0.0 } else {
                                //      accept_samples / wmc_accept
                                // }
                                // // proposal 3: use only the joint of phi && a, but then divide by _weight only_ which is:
                                // // the joint of a && s divided by the space of samples s
                                // if wmc_sample == 0.0 { 0.0 } else {
                                //      accept_samples / wmc_sample
                                // }
                                // proposal 4: the accepting criteria is a & s. Normalize the query.
                                // Weight the query by ratio of s : a & s, but only do this if you sampled (ie: wmc(s) < 1)
                                // the funny thing is that the accepting formula includes both un- and partially- collapsed vars
                                // this is kind of a departure from Friedman & Koller (although not quite, need to review that)
                                // if wmc_sample == 1.0 { 1.0 } else if accept_samples == 0.0 { 0.0 } else {
                                if accept_samples == 0.0 { 0.0 } else {
                                     accept_samples / wmc_sample
                                }
                            // })
                            // .collect_vec();
                        ;


                        // // proposal 1 & 2: leave standard WMC as-is
                        // (wmc_numa_dena, ws)
                        // // proposal 3: only compute the joint of ss && phi && a:
                        // (wmc_nums, ws)
                        // proposal 4: the accepting criteria is a & s. Normalize the query.
                        // (wmc_numas_denas, ws)
                        let query = wmc_numas_denas;

                        println!("current plan:");
                        println!("         query: {:?}", query);
                        println!("*           wr: {:?}", w);
                        (query, w)
                    }
                    Query::SQuery(_) => {
                        // FIXME: ensure that you're not missing something about incorporating the accepting criteria into the weight!
                        let qs = out.sample.out.iter().fold(vec![], |mut acc, v| {
                            let vs: Vec<f64> = From::<&SVal>::from(v);
                            acc.extend(vs);
                            acc
                        });
                        println!("         query: {:?}", qs);
                        println!("weight        : {} = {}", pq.render(), pq.weight());
                        // let ws = qs.iter().map(|_| pq.weight()).collect_vec();
                        (qs, pq.weight())
                    }
                };
                e.add(Exp1::new(ws, query));
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
