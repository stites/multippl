use crate::data::HashMap;
use crate::inference::debug::*;
use crate::inference::wmc::*;
use crate::typeinf::grammar::*;
use crate::uniquify::grammar::*;
use crate::utils::render::*;
use crate::*;
use itertools::Either;
use itertools::*;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::repr::wmc::{RealSemiring, WmcParams};
use tracing::*;

fn wmc_helper(
    mgr: &mut Mgr,
    wmc_final_accept: f64,
    final_accept: BddPtr,
    d: BddPtr,
    params: &WmcParams<RealSemiring>,
) -> f64 {
    if wmc_final_accept == 0.0 {
        0.0
    } else {
        let num = mgr.and(d, final_accept);
        let RealSemiring(a) = num.wmc(mgr.get_order(), params);
        a / wmc_final_accept
    }
}

pub fn importance_weighting(steps: usize, p: &str) -> Either<Vec<f64>, Vec<Vec<f64>>> {
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
) -> (Either<Vec<f64>, Vec<Vec<f64>>>, Option<WmcStats>) {
    let (mgr, p, lenv) = make_mgr_and_ir(code).unwrap();
    let mut rng = opt.rng();
    let mut e = None;
    let mut wmc = WmcP::new_with_size(lenv.lblsym as usize);

    debug!("running with options: {:#?}", opt);
    for step in 1..=steps {
        if step % 100 == 1 {
            debug!("step: {step}");
        }
        let mut mgr = crate::data::new_manager(0);
        match crate::runner(&mut mgr, &mut rng, wmc, opt, &p, &lenv) {
            Ok(o) => {
                let (out, lw) = (o.out, o.weight);
                trace!("sample output : {:?}", out.sample.out);
                trace!("exact output  : {:?}", out.exact.out);
                trace!("accepting     : {:?}", out.exact.accept);
                let params = o.wmcp.params();
                let samples = out.exact.samples(&mut mgr);
                let accept = out.exact.accept;
                let final_accept = mgr.and(samples, accept);
                let RealSemiring(wmc_accept) = accept.wmc(mgr.get_order(), params);
                let RealSemiring(wmc_final_accept) = final_accept.wmc(mgr.get_order(), params);
                let RealSemiring(wmc_sample) = samples.wmc(mgr.get_order(), params);

                let lwmc_accept = Ln::new(wmc_accept);
                let lwmc_final_accept = Ln::new(wmc_final_accept);
                let lwmc_sample = Ln::new(wmc_sample);

                match p.query() {
                    // TODO drop this traversal, pre-compute during eval
                    Query::EQuery(_) => {
                        // the final accepting criteria is a & s. Normalize the query.
                        match out.exact.dists() {
                            EDists::Bdds(Bdds { bdds: ds }) => {
                                let query = ds
                                    .iter()
                                    .map(|d| {
                                        wmc_helper(
                                            &mut mgr,
                                            wmc_final_accept,
                                            final_accept,
                                            *d,
                                            params,
                                        )
                                    })
                                    .collect_vec();
                                wmc = o.wmcp;
                                if e.is_none() {
                                    e = Some(Either::Left(Exp1::empty()));
                                }
                                let mut lexp = e.as_mut().unwrap().as_mut().left();
                                let exp = lexp.as_mut().unwrap();
                                exp.add_f64(Exp1::<f64>::new(lw, query));
                            }
                            EDists::Prds(Prds { prods: dds }) => {
                                let prodquery = dds
                                    .iter()
                                    .map(|ds| {
                                        ds.iter()
                                            .map(|d| {
                                                wmc_helper(
                                                    &mut mgr,
                                                    wmc_final_accept,
                                                    final_accept,
                                                    *d,
                                                    params,
                                                )
                                            })
                                            .collect_vec()
                                    })
                                    .collect_vec();

                                wmc = o.wmcp;
                                if e.is_none() {
                                    e = Some(Either::Right(Exp1::empty()));
                                }
                                let mut rexp = e.as_mut().unwrap().as_mut().right();
                                let exp = rexp.as_mut().unwrap();
                                exp.add_vec(Exp1::<Vec<f64>>::new(lw, prodquery));
                            }
                        }
                    }
                    Query::SQuery(_) => {
                        let query = out.sample.out.iter().fold(vec![], |mut acc, v| {
                            let vs: Vec<f64> = From::<&SVal>::from(v);
                            acc.extend(vs);
                            acc
                        });
                        trace!("           query: {:?}", query);
                        trace!("-----------------------");
                        wmc = o.wmcp;
                        if e.is_none() {
                            e = Some(Either::Left(Exp1::empty()));
                        }
                        let mut lexp = e.as_mut().unwrap().as_mut().left();
                        let exp = lexp.as_mut().unwrap();
                        exp.add_f64(Exp1::<f64>::new(lw, query));
                    }
                }
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
    (
        e.unwrap()
            .map_either(|x| Exp1::<f64>::query(&x), |x| Exp1::<Vec<f64>>::query(&x)),
        None,
    )
}
