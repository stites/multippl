use crate::inference::debug::*;
use crate::inference::wmc::*;
use crate::uniquify::grammar::*;
use crate::utils::render::*;
use crate::*;
use itertools::*;
use std::collections::HashMap;
use tracing::debug;

pub fn importance_weighting(steps: usize, p: &ProgramInferable) -> Vec<f64> {
    importance_weighting_h(
        steps,
        p,
        &crate::Options {
            opt: true,
            ..Default::default()
        },
    )
    .0
}

#[allow(unused_mut)]
pub fn importance_weighting_h(
    steps: usize,
    p: &ProgramInferable,
    opt: &crate::Options,
) -> (Vec<f64>, WmcStats) {
    let mut e = Expectations::empty();
    let mut ws: Vec<f64> = vec![];
    let mut qss: Vec<Vec<Probability>> = vec![];
    let mut prs: Vec<Vec<f64>> = vec![];
    let mut sss: Vec<HashMap<UniqueId, Vec<bool>>> = vec![];
    let mut mgr = crate::make_mgr(p);
    let mut is_debug: bool = false;

    let mut compilations = vec![];
    let mut compilations_ws = vec![];
    let mut compilations_prs = vec![];
    let mut compilations_qs = vec![];
    let mut stats_max = None;
    debug!("running with options: {:#?}", opt);
    // let mut debug_inv = None;

    for _step in 1..=steps {
        match crate::runner_with_stdrng(p, &mut mgr, opt) {
            Ok((cs, inv, _)) => {
                is_debug = match cs {
                    Compiled::Output(_) => false,
                    Compiled::Debug(_) => {
                        compilations = cs.clone().into_iter().collect();
                        // debug_inv = Some(inv.clone());
                        true
                    }
                };
                cs.into_iter().for_each(|c| {
                    let ps;
                    let stats;
                    (ps, stats) = wmc_prob(&mut mgr, &c);

                    let w = c.importance;
                    debug!("{}", c.accept.print_bdd());
                    debug!("{}", renderbdds(&c.dists));
                    debug!(
                        "{}, {}, {}",
                        w.weight(),
                        renderfloats(&ps, false),
                        prs.len()
                    );
                    let exp_cur = Expectations::new(w.clone(), ps.clone());
                    e = Expectations::add(e.clone(), exp_cur);
                    ws.push(w.weight());
                    // qss.push(_qs);
                    prs.push(ps);
                    // sss.push(env.samples.clone());
                    qss.push(c.probabilities);
                    stats_max = stats_max
                        .as_ref()
                        .map(|prv: &WmcStats| prv.largest_of(&stats))
                        .or_else(|| Some(stats));

                    if is_debug {
                        compilations_ws.push(ws.clone());
                        compilations_prs.push(prs.clone());
                        compilations_qs.push(qss.clone());
                    }
                });
                if is_debug {
                    break;
                }
            }
            Err(e) => panic!("{:?}", e),
        }
    }

    let exp = e.exp.clone();
    let expw = e.expw.clone();
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    debug!(
        exp = renderfloats(&exp, false),
        expw = renderfloats(&expw, false)
    );

    if is_debug {
        let final_ws: Vec<f64> = compilations_ws.last().unwrap().to_vec();
        debug!("weights: {:?}", final_ws);
        for (i, w) in final_ws.iter().enumerate() {
            debug!("{}: {:?}", i, w);
        }
        let final_ps: Vec<Vec<f64>> = (compilations_prs.last().unwrap()).to_vec();
        debug!("probabilities: {:?}", final_ps);
        for (i, p) in final_ps.iter().enumerate() {
            debug!("{}: {:?}", i, p);
        }
        let final_qs: Vec<Vec<Probability>> = (compilations_qs.last().unwrap()).to_vec();
        debug!("proposed probs: {:?}", final_qs);
        for (i, p) in final_qs.iter().enumerate() {
            debug!("{}: {:?}", i, p);
        }
        debug!("expectations:");
        debug!("{:#?}", e);

        debug!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        debug!("compilations:");
        for (i, c) in compilations.iter().enumerate() {
            debug_compiled!(c);
        }
        debug!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        debug!("~ WARNING! DEBUG WILL NOT PRUNE UNNECESSARY SAMPLES!!! ~");
        debug!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");

        let x =
            compilations
                .iter()
                .fold(vec![0.0; compilations[0].probabilities.len()], |agg, c| {
                    let azs;
                    (azs, _) = wmc_prob(&mut mgr, &c);

                    izip!(agg, c.probabilities.clone(), azs)
                        .map(|(fin, q, wmc)| fin + q.as_f64() * c.importance.weight() * wmc)
                        .collect_vec()
                });
        (x, stats_max.expect("at least one sample"))
    } else {
        debug_importance_weighting(true, steps, &ws, &[], &prs, &sss, &exp, &expw);
        // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
        let x = izip!(exp, expw)
            .map(|(exp, expw)| if exp == 0.0 { 0.0 } else { exp / expw })
            .collect_vec();
        (x, stats_max.expect("at least one sample"))
    }
}
