use crate::inference::debug::*;
use crate::inference::wmc::*;
use crate::uniquify::grammar::*;
use crate::utils::render::*;
use crate::*;
use itertools::*;
use std::collections::HashMap;
use rsdd::builder::bdd_builder::DDNNFPtr;
use tracing::*;
use rsdd::repr::wmc::RealSemiring;

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
    let (mut mgr, p, lenv) = make_mgr_and_ir(code).unwrap();
    let mut rng = opt.rng();
    let mut e = Exp1::empty();

    debug!("running with options: {:#?}", opt);
    for step in 1..=steps {
        if step % 100 == 1 {
            debug!("step: {step}");
        }
        match crate::runner(&mut mgr, &mut rng, &opt, &p, &lenv) {
            Ok(o) => {
                trace!("{:?}", o.out.exact.out);
                let (out, pq) = (o.out, o.pq);
                debug!("sample output : {:?}", out.sample.out);
                debug!("sample trace  : {:?}", out.sample.trace);
                debug!("exact output  : {:?}", out.exact.out);
                debug!("accepting     : {:?}", out.exact.accept);
                let (query, w): (Vec<f64>, f64) = match p.query() { // TODO drop this traversal, pre-compute during eval
                    Query::EQuery(_) => {
                        let qs = numerators(&mut mgr, &out.exact);
                        debug!("         query: {:?}", qs);
                        let var_order = mgr.get_order().clone();
                        let params = out.exact.weightmap.as_params(mgr.get_order().num_vars() as u64);

                        let final_samples = out.exact.samples(&mut mgr);
                        let final_accept = mgr.and(final_samples, out.exact.accept);
                        let RealSemiring(wmc_accept) = final_accept.wmc(&var_order, &params);
                        debug!("wmc accept / w: {:?}", wmc_accept);
                        (qs, wmc_accept )
                    },
                    Query::SQuery(_) => {
                        let qs = out.sample.out.iter().fold(vec![], |mut acc, v| {
                            let vs: Vec<f64> = From::<&SVal>::from(v);
                            acc.extend(vs);
                            acc
                        });
                        debug!("         query: {:?}", qs);
                        debug!("weight        : {} = {}", pq.render(), pq.weight());
                        (qs, pq.weight())
                    }
                };
                e.add(Exp1::new(w, query));
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
    debug!("       ∑[w]: {}", e.sum_w);

    // // debug_importance_weighting(true, steps, &ws, &[], &prs, &sss, &exp, &expw);

    // let x = e.query();
    // info!("E[q]: {}", renderfloats(&x, false));
    // (x, stats_max)
    (e.query(), None)
}
