use crate::inference::debug::*;
use crate::inference::wmc::*;
use crate::uniquify::grammar::*;
use crate::utils::render::*;
use crate::*;
use itertools::*;
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
    let mut mgr = crate::make_mgr(code).unwrap();
    let mut rng = opt.rng();
    let mut e = Expectations::empty();
    let mut stats_max = None;
    debug!("running with options: {:#?}", opt);
    for step in 1..=steps {
        if step % 100 == 1 {
            debug!("step: {step}");
        }
        match crate::runner(code, &mut mgr, &mut rng, opt) {
            Ok(o) => {
                trace!("{:?}", o.out.exact.out);
                let (out, p, pq) = (o.out, o.prg, o.pq);
                debug!("sample output : {}", rendersvals(&out.sample.out));
                debug!("exact output  : {}", renderbdds(&out.exact.out));
                debug!("accepting     : {:?}", out.exact.accept);
                // debug!("computed probs: {:?}", ps);
                debug!("weight        : {} = {}", pq.render(), pq.weight());
                let (query, stats): (Vec<f64>, Option<WmcStats>) = match p.query() {
                    Query::EQuery(_) => wmc_prob(&mut mgr, &out.exact),
                    Query::SQuery(_) => (
                        out.sample.out.iter().fold(vec![], |mut acc, v| {
                            let vs: Vec<f64> = From::<&SVal>::from(v);
                            acc.extend(vs);
                            acc
                        }),
                        None,
                    ),
                };
                let exp_cur = Expectations::new(pq, query);
                e = Expectations::add(e.clone(), exp_cur);
                stats_max = stats_max
                    .as_ref()
                    .map(|prv: &WmcStats| {
                        prv.largest_of(&stats.expect("some exact compilation should occur"))
                    })
                    .or_else(|| stats);
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

    let exp = e.exp.clone();
    let expw = e.expw.clone();
    debug!(
        exp = renderfloats(&exp, false),
        expw = renderfloats(&expw, false)
    );

    // debug_importance_weighting(true, steps, &ws, &[], &prs, &sss, &exp, &expw);

    let x = izip!(exp, expw)
        .map(|(exp, expw)| if exp == 0.0 { 0.0 } else { exp / expw })
        .collect_vec();
    (x, stats_max)
}
