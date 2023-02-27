use crate::compile::*;
use crate::grammar::*;
use crate::render::*;
use crate::run;
use crate::typecheck::grammar::ProgramTyped;
use crate::uniquify::grammar::*;
use crate::*;
use itertools::*;
use rayon::iter::*;
use rayon::prelude::*;
use rsdd::repr::bdd::*;
use rsdd::repr::ddnnf::DDNNFPtr;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;
use std::iter::Sum;
use tracing::debug;

pub fn calculate_wmc_prob<T: Copy + std::fmt::Debug + num_traits::Num + std::fmt::Display>(
    mgr: &mut Mgr,
    params: &WmcParams<T>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
) -> (T, T) {
    let num = mgr.and(dist, accept);
    debug!(
        "{} /\\ {} = {}",
        dist.print_bdd(),
        accept.print_bdd(),
        num.print_bdd()
    );
    debug!("-------------");
    debug!("{}", accept.print_bdd());

    let a = num.wmc(&var_order, &params);
    let z = accept.wmc(&var_order, &params);
    debug!("{}", a);
    debug!("----------- = {}", a / z);
    debug!("{}", z);
    (a, z)
}

pub fn wmc_prob(mgr: &mut Mgr, c: &Output) -> Vec<(f64, f64)> {
    c.dists
        .iter()
        .map(|d| {
            calculate_wmc_prob(
                mgr,
                &c.weightmap.as_params(mgr.get_order().num_vars() as u64),
                &mgr.get_order().clone(),
                *d,
                c.accept,
            )
        })
        .collect_vec()
}

#[inline]
pub fn get_vec<T: Copy>(v: Vec<T>, i: usize) -> Option<T> {
    if i < v.len() {
        Some(v[i])
    } else {
        None
    }
}
#[inline]
pub fn get_or_else<T: Copy>(v: Vec<T>, i: usize, d: T) -> T {
    get_vec(v, i).unwrap_or_else(|| d)
}
pub fn exact_inf(p: &ProgramTyped) -> Vec<f64> {
    match crate::run(p) {
        Ok((mut mgr, c)) => {
            let azs = wmc_prob(&mut mgr, &c);
            azs.into_iter()
                .map(|(a, z)| {
                    //   debug!(a = a, z = z);
                    a / z
                })
                .collect_vec()
        }
        Err(e) => panic!(
            "\nCompiler Error!!!\n==============\n{}\n==============\n",
            e.to_string()
        ),
    }
}

fn debug_importance_weighting(
    high_precision: bool,
    steps: usize,
    ws: &Vec<f64>,
    qss: &Vec<Vec<f64>>,
    pss: &Vec<Vec<f64>>,
    ss: &Vec<HashMap<UniqueId, Vec<bool>>>,
    exp: &Vec<f64>,
    expw: &Vec<f64>,
) {
    if steps < 1001 {
        debug!("");
        debug!("ws = {}", render_weights(ws, false));
        debug!("qs = {}", render_history(qss, false));
        debug!("ps = {}", render_history(pss, false));
        debug!(
            "ss = [{}]",
            ss.iter()
                .map(|kvs| format!(
                    "[{}]",
                    kvs.iter()
                        .map(|(k, vs)| format!("{:?}:{:?}", k, vs))
                        .join(",")
                ))
                .join(", ")
        );
    }
    debug!("ws    = {}", ws.iter().sum::<f64>());
    debug!(exp = renderfloats(exp, false));
    debug!(expw = renderfloats(expw, false));
    izip!(exp, expw).enumerate().for_each(|(i, (exp, expw))| {
        let num = format!("{}", exp);
        let denom = format!("{}", expw);
        let num_len: i32 = num.len().try_into().unwrap();
        let denom_len: i32 = denom.len().try_into().unwrap();

        debug!("computed:");
        debug!("{}", num);
        debug!("{} = {:.4}", "-".repeat(num.len()), (exp / expw));
        let leftpad: usize = ((num_len - denom_len).abs() / 2).try_into().unwrap();
        debug!("{}{}", " ".repeat(leftpad), denom);
        debug!("");
    });
}
pub fn importance_weighting_inf(steps: usize, p: &ProgramTyped) -> Vec<f64> {
    importance_weighting_inf_h(steps, p, &Default::default())
}
pub fn importance_weighting_inf_h(
    steps: usize,
    p: &ProgramTyped,
    opt: &crate::Options,
) -> Vec<f64> {
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

    for _step in 1..=steps {
        match crate::runner_h(p, &mut mgr, opt) {
            Ok(cs) => {
                is_debug = match cs {
                    Compiled::Output(_) => false,
                    Compiled::Debug(_) => {
                        compilations = cs.clone().into_iter().collect();
                        true
                    }
                };
                cs.into_iter().for_each(|c| {
                    let azs = wmc_prob(&mut mgr, &c);
                    let ps = azs
                        .into_iter()
                        .map(|(a, z)| {
                            debug!(
                                a = a,
                                z = z,
                                accept = c.accept.print_bdd(),
                                dists = renderbdds(&c.dists)
                            );
                            if a == z && a == 0.0 {
                                0.0
                            } else {
                                (a / z) as f64
                            }
                        })
                        .collect_vec();

                    let w = c.importance.weight();
                    debug!("{}", c.accept.print_bdd());
                    debug!("{}", renderbdds(&c.dists));
                    debug!("{}, {}, {}", w, renderfloats(&ps, false), prs.len());
                    let exp_cur = Expectations::new(w, ps.clone());
                    e = Expectations::add(e.clone(), exp_cur);
                    ws.push(w);
                    // qss.push(_qs);
                    prs.push(ps);
                    // sss.push(env.samples.clone());
                    qss.push(c.probabilities.clone());

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

    let exp = e.clone().exp.clone();
    let expw = e.clone().expw.clone();
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
        let final_ps: Vec<Vec<f64>> = (compilations_prs.last().unwrap())
            .into_iter()
            .cloned()
            .collect();
        debug!("probabilities: {:?}", final_ps);
        for (i, p) in final_ps.iter().enumerate() {
            debug!("{}: {:?}", i, p);
        }
        let final_qs: Vec<Vec<Probability>> = (compilations_qs.last().unwrap())
            .into_iter()
            .cloned()
            .collect();
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
        debug!("~ WARNING! DEBUG WILL NOT PRUNE UNNESSECARY SAMPLES!!! ~");
        debug!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");

        compilations
            .iter()
            .fold(vec![0.0; compilations[0].probabilities.len()], |agg, c| {
                let azs = wmc_prob(&mut mgr, &c);
                izip!(agg, c.probabilities.clone(), azs)
                    .map(|(fin, q, (a, z))| fin + q.as_f64() * c.importance.weight() * a / z)
                    .collect_vec()
            })
    } else {
        debug_importance_weighting(true, steps, &ws, &vec![], &prs, &sss, &exp, &expw);
        // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
        let res = izip!(exp.clone(), expw.clone())
            .map(|(exp, expw)| (exp / expw) as f64)
            .collect_vec();
        res
    }
}

// fn conc_prelude(env: &mut Env, p: &ProgramTyped) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
//     match run(env, p) {
//         Ok(c) => {
//             let azs = wmc_prob(env, &c);
//             let prs = azs
//                 .into_iter()
//                 .map(|(a, z)| {
//                     debug!(
//                         a = a,
//                         z = z,
//                         accept = c.accept.print_bdd(),
//                         dists = renderbdds(&c.dists)
//                     );
//                     if a == z && a == 0.0 {
//                         0.0
//                     } else {
//                         (a / z) as f64
//                     }
//                 })
//                 .collect_vec();
//             let _qs = c
//                 .probabilities
//                 .iter()
//                 .map(Probability::as_f64)
//                 .collect_vec();
//             let w = c.importance.weight();

//             let mut exp = vec![0.0; prs.len()];
//             let mut expw = vec![0.0; prs.len()];
//             let mut expw2 = vec![0.0; prs.len()];
//             izip!(prs.clone(), _qs.clone())
//                 .enumerate()
//                 .for_each(|(i, (pr, _q))| {
//                     exp[i] = exp[i] + w * pr;
//                     expw[i] = expw[i] + w;
//                     expw2[i] = expw2[i] + (w * w);
//                 });
//             return (exp, expw, expw2);
//         }
//         Err(e) => panic!("{:?}", e),
//     }
// }

#[derive(Debug, Clone)]
pub struct Expectations {
    pub exp: Vec<f64>,
    pub expw: Vec<f64>,
    // pub expw2: Vec<f64>,
}
impl Expectations {
    pub fn empty() -> Self {
        Self {
            exp: vec![],
            expw: vec![],
        }
    }
    pub fn new(w: f64, prs: Vec<f64>) -> Self {
        let (exp, expw) = prs
            .into_iter()
            .map(|pr| {
                let exp = w * pr;
                let expw = w;
                let expw2 = w * w;
                (exp, expw) // , expw2)
            })
            .unzip();
        Self { exp, expw }
    }
    pub fn add(l: Self, o: Self) -> Self {
        if l.exp.len() == 0 {
            o
        } else {
            Self {
                exp: izip!(l.exp, o.exp).map(|(l, r)| l + r).collect_vec(),
                expw: izip!(l.expw, o.expw).map(|(l, r)| l + r).collect_vec(),
            }
        }
    }
}
// pub trait Sum<A = Self> {
//     fn sum<I>(iter: I) -> Self
//     where
//         I: Iterator<Item = A>;
// }

impl Sum for Expectations {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Expectations>,
    {
        iter.fold(Expectations::empty(), Expectations::add)
    }
}

// pub fn importance_weighting_inf_conc(
//     _envargs: &EnvArgs,
//     steps: usize,
//     p: &ProgramTyped,
// ) -> Vec<f64> {
//     // let mut exp = vec![];
//     // let mut expw = vec![];
//     let fin: Expectations = (1..=steps)
//         .into_par_iter()
//         .map(|_step| {
//             let mut enva = EnvArgs::default_args(None);
//             let mut env = Env::from_args(&mut enva);
//             match run(&mut env, p) {
//                 Ok(c) => {
//                     let azs = wmc_prob(&mut env, &c);
//                     let prs = azs
//                         .into_iter()
//                         .map(|(a, z)| {
//                             debug!(
//                                 a = a,
//                                 z = z,
//                                 accept = c.accept.print_bdd(),
//                                 dists = renderbdds(&c.dists)
//                             );
//                             if a == z && a == 0.0 {
//                                 0.0
//                             } else {
//                                 (a / z) as f64
//                             }
//                         })
//                         .collect_vec();
//                     let _qs = c
//                         .probabilities
//                         .iter()
//                         .map(Probability::as_f64)
//                         .collect_vec();
//                     let w = c.importance.weight();

//                     return Ok(Expectations::new(w, prs));
//                     // let mut exp = vec![0.0; prs.len()];
//                     // let mut expw = vec![0.0; prs.len()];
//                     // let mut expw2 = vec![0.0; prs.len()];
//                     // izip!(prs.clone(), _qs.clone())
//                     //     .enumerate()
//                     //     .for_each(|(i, (pr, _q))| {
//                     //         exp[i] = exp[i] + w * pr;
//                     //         expw[i] = expw[i] + w;
//                     //         expw2[i] = expw2[i] + (w * w);
//                     //     });
//                     // return Ok((exp, expw)); // , expw2)
//                 }
//                 Err(e) => Err(e),
//             }
//         })
//         .filter_map(|x| x.ok())
//         .sum()
//         // .map(|(l, r)| (l[0], r[0]))
//         // .unzip_into_vecs(&mut exp, &mut expw)
//         ;
//     // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
//     // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
//     let exp = fin.exp;
//     let expw = fin.expw;
//     izip!(exp, expw)
//         .map(|(exp, expw)| (exp / expw) as f64)
//         .collect_vec()
// }

// pub fn importance_weighting_inf_seeded(
//     seeds: Vec<u64>,
//     steps: usize,
//     p: &ProgramTyped,
// ) -> Vec<f64> {
//     let (mut exp, mut expw, mut expw2) = (vec![], vec![], vec![]);
//     let mut ws: Vec<f64> = vec![];
//     let mut qss: Vec<Vec<f64>> = vec![];
//     let mut pss: Vec<Vec<f64>> = vec![];
//     let mut sss: Vec<HashMap<UniqueId, Vec<bool>>> = vec![];
//     let num_seeds = seeds.len();

//     for step in 1..=steps {
//         let seed = seeds[step % num_seeds];
//         let mut envargs = EnvArgs::default_args(Some(seed));

//         let mut env = Env::from_args(&mut envargs);
//         match run(&mut env, p) {
//             Ok(c) => {
//                 let azs = wmc_prob(&mut env, &c);
//                 let prs = azs
//                     .into_iter()
//                     .map(|(a, z)| {
//                         debug!(a = a, z = z);
//                         (a / z) as f64
//                     })
//                     .collect_vec();
//                 let _qs = c
//                     .probabilities
//                     .iter()
//                     .map(Probability::as_f64)
//                     .collect_vec();
//                 let w = c.importance.weight();

//                 if exp.len() == 0 {
//                     exp = vec![0.0; prs.len()];
//                     expw = vec![0.0; prs.len()];
//                     expw2 = vec![0.0; prs.len()];
//                 }
//                 izip!(prs.clone(), _qs.clone())
//                     .enumerate()
//                     .for_each(|(i, (pr, _q))| {
//                         exp[i] = exp[i] + w * pr;
//                         expw[i] = expw[i] + w;
//                         expw2[i] = expw2[i] + (w * w);
//                     });
//                 ws.push(w);
//                 qss.push(_qs);
//                 pss.push(prs);
//                 sss.push(env.samples.clone());
//             }
//             Err(e) => panic!("{:?}", e),
//         }
//     }
//     debug_importance_weighting(true, steps, &ws, &qss, &pss, &sss, &exp, &expw);
//     izip!(exp, expw)
//         .map(|(exp, expw)| (exp / expw) as f64)
//         .collect_vec()
// }
