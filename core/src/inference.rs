use crate::analysis::grammar::DecoratedVar;
use crate::annotate::grammar::Var;
use crate::compile::*;
use crate::grammar::*;
use crate::render::*;
use crate::run;
use crate::typecheck::grammar::ProgramTyped;
use crate::uniquify::grammar::*;
use crate::*;
use itertools::*;
use rsdd::repr::bdd::*;
use rsdd::repr::ddnnf::DDNNFPtr;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use rsdd::sample::probability::Probability;
use std::cmp;
use std::collections::HashMap;
use std::iter::Sum;
use tracing::debug;

pub struct WmcStats {
    pub dist: usize,
    pub accept: usize,
    pub dist_accept: usize,
    pub mgr_recursive_calls: usize,
}
impl WmcStats {
    fn largest_of(&self, o: &WmcStats) -> WmcStats {
        WmcStats {
            dist: cmp::max(self.dist, o.dist),
            accept: cmp::max(self.accept, o.accept),
            dist_accept: cmp::max(self.dist_accept, o.dist_accept),
            mgr_recursive_calls: cmp::max(self.mgr_recursive_calls, o.mgr_recursive_calls),
        }
    }
}
pub fn calculate_wmc_prob_h(
    mgr: &mut Mgr,
    params: &WmcParams<f64>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
) -> (f64, f64, WmcStats) {
    let num = mgr.and(dist, accept);
    debug!(
        "{} /\\ {} = {}",
        dist.print_bdd(),
        accept.print_bdd(),
        num.print_bdd()
    );
    debug!("-------------");
    debug!("{}", accept.print_bdd());

    let a = num.wmc(var_order, params);
    let z = accept.wmc(var_order, params);
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
    params: &WmcParams<f64>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
) -> (f64, WmcStats) {
    let (a, z, stats) = calculate_wmc_prob_h(mgr, params, var_order, dist, accept);
    if a == z && a == 0.0 {
        (0.0, stats)
    } else {
        (a / z, stats)
    }
}

pub fn calculate_wmc_prob(
    mgr: &mut Mgr,
    params: &WmcParams<f64>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
    samples: BddPtr,
) -> (f64, WmcStats) {
    let accept = mgr.and(samples, accept);
    calculate_wmc_prob_hf64(mgr, params, var_order, dist, accept)
}

pub fn calculate_wmc_prob_opt(
    mgr: &mut Mgr,
    params: &WmcParams<f64>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
    samples_opt: &HashMap<BddPtr, (Option<Var>, bool)>,
    sampling_context: Option<DecoratedVar>,
) -> (f64, WmcStats) {
    let removable = sampling_context
        .map(|dv| dv.below.difference(&dv.above).cloned().collect())
        .unwrap_or(std::collections::HashSet::new());
    let compiled_samples = samples_opt.iter().fold(
        BddPtr::PtrTrue,
        |formula, (bdd, (dv, sampled_value))| match dv {
            None => formula,
            Some(var) => {
                if removable.contains(&var) {
                    formula
                } else {
                    let dist_holds = mgr.iff(*bdd, BddPtr::from_bool(*sampled_value));
                    mgr.and(formula, dist_holds)
                }
            }
        },
    );
    let accept = mgr.and(compiled_samples, accept);
    calculate_wmc_prob_hf64(mgr, params, var_order, dist, accept)
}

pub fn wmc_prob(mgr: &mut Mgr, c: &Output) -> (Vec<f64>, WmcStats) {
    let mut last_stats = None;
    let probs = c
        .dists
        .iter()
        .map(|d| {
            let (p, stats) = calculate_wmc_prob(
                mgr,
                &c.weightmap.as_params(mgr.get_order().num_vars() as u64),
                &mgr.get_order().clone(),
                *d,
                c.accept,
                c.samples,
            );
            last_stats = Some(stats);
            p
        })
        .collect_vec();
    (probs, last_stats.expect("output dists should be non-empty"))
}

pub fn wmc_prob_opt(
    mgr: &mut Mgr,
    c: &Output,
    sampling_context: Option<DecoratedVar>,
) -> (Vec<f64>, WmcStats) {
    let mut prev = None;
    let probs = c
        .dists
        .iter()
        .map(|d| {
            let (p, stats) = calculate_wmc_prob_opt(
                mgr,
                &c.weightmap.as_params(mgr.get_order().num_vars() as u64),
                &mgr.get_order().clone(),
                *d,
                c.accept,
                &c.samples_opt,
                sampling_context.clone(),
            );
            prev = prev
                .as_ref()
                .map(|prv: &WmcStats| prv.largest_of(&stats))
                .or_else(|| Some(stats));
            p
        })
        .collect_vec();
    (probs, prev.expect("must include at least one output"))
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
    get_vec(v, i).unwrap_or(d)
}
pub fn exact_with(p: &ProgramTyped) -> (Vec<f64>, WmcStats) {
    match crate::run(p) {
        Ok((mut mgr, c)) => wmc_prob(&mut mgr, &c),
        Err(e) => panic!(
            "\nCompiler Error!!!\n==============\n{}\n==============\n",
            e
        ),
    }
}

pub fn exact(p: &ProgramTyped) -> Vec<f64> {
    exact_with(p).0
}

#[allow(clippy::too_many_arguments)]
fn debug_importance_weighting(
    high_precision: bool,
    steps: usize,
    ws: &[f64],
    qss: &[Vec<f64>],
    pss: &[Vec<f64>],
    ss: &[HashMap<UniqueId, Vec<bool>>],
    exp: &[f64],
    expw: &[f64],
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
pub fn importance_weighting(steps: usize, p: &ProgramTyped) -> Vec<f64> {
    importance_weighting_h(steps, p, &Default::default()).0
}
#[allow(unused_mut)]
pub fn importance_weighting_h(
    steps: usize,
    p: &ProgramTyped,
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
                    let ps;
                    let stats;
                    // if opt.opt {
                    //     (ps, stats) = wmc_prob_opt(&mut mgr, &c);
                    // } else {
                    //     (ps, stats) = wmc_prob(&mut mgr, &c);
                    // }
                    (ps, stats) = wmc_prob(&mut mgr, &c);

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
                    let (azs, _) = wmc_prob(&mut mgr, c);
                    izip!(agg, c.probabilities.clone(), azs)
                        .map(|(fin, q, wmc)| fin + q.as_f64() * c.importance.weight() * wmc)
                        .collect_vec()
                });
        (x, stats_max.expect("at least one sample"))
    } else {
        debug_importance_weighting(true, steps, &ws, &[], &prs, &sss, &exp, &expw);
        // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
        let x = izip!(exp, expw).map(|(exp, expw)| exp / expw).collect_vec();
        (x, stats_max.expect("at least one sample"))
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
        if l.exp.is_empty() {
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

// pub fn importance_weighting_conc(
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

// pub fn importance_weighting_seeded(
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
