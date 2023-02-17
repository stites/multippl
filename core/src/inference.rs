use crate::grammar::*;
use crate::semantics::*;
use itertools::*;
use rayon::iter::*;
use rayon::prelude::*;
use rsdd::repr::bdd::*;
use rsdd::repr::ddnnf::DDNNFPtr;
use rsdd::repr::var_order::VarOrder;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;
use std::iter::Sum;
use tracing::debug;

pub fn _wmc_prob(env: &mut Env, m: &WeightMap, dist: BddPtr, accept: BddPtr) -> (f64, f64) {
    let (params, mx) = weight_map_to_params(m);
    let var_order = VarOrder::linear_order(mx as usize);
    let a = env.mgr.and(dist, accept).wmc(&var_order, &params);
    let z = accept.wmc(&var_order, &params);
    (a, z)
}
pub fn wmc_prob(env: &mut Env, c: &Compiled) -> Vec<(f64, f64)> {
    c.dists
        .iter()
        .map(|d| _wmc_prob(env, &c.weight_map, *d, c.accept))
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
pub fn exact_inf(env: &mut Env, p: &Program) -> Vec<f64> {
    match compile(env, p) {
        Ok(c) => {
            let azs = wmc_prob(env, &c);
            azs.into_iter()
                .map(|(a, z)| {
                    debug!(a = a, z = z);
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

fn render_weights(ws: &Vec<f64>, high_precision: bool) -> String {
    ws.iter()
        .cloned()
        .map(|w| format!("[{}]", fmt_f64(high_precision)(w)))
        .join(", ")
}

fn render_history(xss: &Vec<Vec<f64>>, high_precision: bool) -> String {
    xss.iter()
        .map(|ws| ws.iter().cloned().map(fmt_f64(high_precision)).join(", "))
        .map(|ws| format!("[{}]", ws))
        .join(", ")
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
        if steps < 11 {
            // debug!("");
            // let num = izip!(ws, qs, ps)
            //     .map(|(w, q, p)| format!("{:.2}*{:.2}*{:.2}", w, q, p))
            //     .join(" + ");
            // let denom = izip!(ws, qs)
            //     .map(|(w, q)| format!("{:.2}*{:.2}", w, q))
            //     .join(" + ");
            // let num_len: i32 = num.len().try_into().unwrap();
            // let denom_len: i32 = denom.len().try_into().unwrap();
            // debug!("{}", num);

            // izip!(exp, expw).enumerate().for_each(|(i, (exp, expw))| {
            //     debug!("{} = {:.4}", "-".repeat(num.len()), (exp / expw));
            // });

            // let leftpad: usize = ((num_len - denom_len).abs() / 2).try_into().unwrap();
            // debug!("{}{}", " ".repeat(leftpad), denom);
        }
    }
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
pub fn importance_weighting_inf(env: &mut Env, steps: usize, p: &Program) -> Vec<f64> {
    let (mut exp, mut expw, mut expw2) = (vec![], vec![], vec![]);
    let mut ws: Vec<f64> = vec![];
    let mut qss: Vec<Vec<f64>> = vec![];
    let mut pss: Vec<Vec<f64>> = vec![];
    let mut sss: Vec<HashMap<UniqueId, Vec<bool>>> = vec![];

    for _step in 1..=steps {
        // FIXME: change back to step
        env.reset_names();
        match compile(env, p) {
            Ok(c) => {
                let azs = wmc_prob(env, &c);

                let prs = azs
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
                let _qs = c
                    .probabilities
                    .iter()
                    .map(Probability::as_f64)
                    .collect_vec();
                let w = c.importance_weight;

                if exp.len() == 0 {
                    exp = vec![0.0; prs.len()];
                    expw = vec![0.0; prs.len()];
                    expw2 = vec![0.0; prs.len()];
                }
                izip!(prs.clone(), _qs.clone())
                    .enumerate()
                    .for_each(|(i, (pr, _q))| {
                        exp[i] = exp[i] + w * pr;
                        expw[i] = expw[i] + w;
                        expw2[i] = expw2[i] + (w * w);
                    });
                ws.push(w);
                qss.push(_qs);
                pss.push(prs);
                sss.push(env.samples.clone());
            }
            Err(e) => panic!("{:?}", e),
        }
    }
    debug_importance_weighting(true, steps, &ws, &qss, &pss, &sss, &exp, &expw);
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    izip!(exp, expw)
        .map(|(exp, expw)| (exp / expw) as f64)
        .collect_vec()
}

fn conc_prelude(env: &mut Env, p: &Program) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    match compile(env, p) {
        Ok(c) => {
            let azs = wmc_prob(env, &c);
            let prs = azs
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
            let _qs = c
                .probabilities
                .iter()
                .map(Probability::as_f64)
                .collect_vec();
            let w = c.importance_weight;

            let mut exp = vec![0.0; prs.len()];
            let mut expw = vec![0.0; prs.len()];
            let mut expw2 = vec![0.0; prs.len()];
            izip!(prs.clone(), _qs.clone())
                .enumerate()
                .for_each(|(i, (pr, _q))| {
                    exp[i] = exp[i] + w * pr;
                    expw[i] = expw[i] + w;
                    expw2[i] = expw2[i] + (w * w);
                });
            return (exp, expw, expw2);
        }
        Err(e) => panic!("{:?}", e),
    }
}

pub struct TestStruct {
    pub exp: Vec<f64>,
    pub expw: Vec<f64>,
    // pub expw2: Vec<f64>,
}
impl TestStruct {
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

impl Sum for TestStruct {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = TestStruct>,
    {
        iter.fold(TestStruct::empty(), TestStruct::add)
    }
}

pub fn importance_weighting_inf_conc(_envargs: &EnvArgs, steps: usize, p: &Program) -> Vec<f64> {
    // let mut exp = vec![];
    // let mut expw = vec![];
    let fin: TestStruct = (1..=steps)
        .into_par_iter()
        .map(|_step| {
            let mut enva = EnvArgs::default_args(None);
            let mut env = Env::from_args(&mut enva);
            match compile(&mut env, p) {
                Ok(c) => {
                    let azs = wmc_prob(&mut env, &c);
                    let prs = azs
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
                    let _qs = c
                        .probabilities
                        .iter()
                        .map(Probability::as_f64)
                        .collect_vec();
                    let w = c.importance_weight;

                    return Ok(TestStruct::new(w, prs));
                    // let mut exp = vec![0.0; prs.len()];
                    // let mut expw = vec![0.0; prs.len()];
                    // let mut expw2 = vec![0.0; prs.len()];
                    // izip!(prs.clone(), _qs.clone())
                    //     .enumerate()
                    //     .for_each(|(i, (pr, _q))| {
                    //         exp[i] = exp[i] + w * pr;
                    //         expw[i] = expw[i] + w;
                    //         expw2[i] = expw2[i] + (w * w);
                    //     });
                    // return Ok((exp, expw)); // , expw2)
                }
                Err(e) => Err(e),
            }
        })
        .filter_map(|x| x.ok())
        .sum()
        // .map(|(l, r)| (l[0], r[0]))
        // .unzip_into_vecs(&mut exp, &mut expw)
        ;
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    let exp = fin.exp;
    let expw = fin.expw;
    izip!(exp, expw)
        .map(|(exp, expw)| (exp / expw) as f64)
        .collect_vec()
}

pub fn importance_weighting_inf_seeded(seeds: Vec<u64>, steps: usize, p: &Program) -> Vec<f64> {
    let (mut exp, mut expw, mut expw2) = (vec![], vec![], vec![]);
    let mut ws: Vec<f64> = vec![];
    let mut qss: Vec<Vec<f64>> = vec![];
    let mut pss: Vec<Vec<f64>> = vec![];
    let mut sss: Vec<HashMap<UniqueId, Vec<bool>>> = vec![];
    let num_seeds = seeds.len();

    for step in 1..=steps {
        let seed = seeds[step % num_seeds];
        let mut envargs = EnvArgs::default_args(Some(seed));

        let mut env = Env::from_args(&mut envargs);
        match compile(&mut env, p) {
            Ok(c) => {
                let azs = wmc_prob(&mut env, &c);
                let prs = azs
                    .into_iter()
                    .map(|(a, z)| {
                        debug!(a = a, z = z);
                        (a / z) as f64
                    })
                    .collect_vec();
                let _qs = c
                    .probabilities
                    .iter()
                    .map(Probability::as_f64)
                    .collect_vec();
                let w = c.importance_weight;

                if exp.len() == 0 {
                    exp = vec![0.0; prs.len()];
                    expw = vec![0.0; prs.len()];
                    expw2 = vec![0.0; prs.len()];
                }
                izip!(prs.clone(), _qs.clone())
                    .enumerate()
                    .for_each(|(i, (pr, _q))| {
                        exp[i] = exp[i] + w * pr;
                        expw[i] = expw[i] + w;
                        expw2[i] = expw2[i] + (w * w);
                    });
                ws.push(w);
                qss.push(_qs);
                pss.push(prs);
                sss.push(env.samples.clone());
            }
            Err(e) => panic!("{:?}", e),
        }
    }
    debug_importance_weighting(true, steps, &ws, &qss, &pss, &sss, &exp, &expw);
    izip!(exp, expw)
        .map(|(exp, expw)| (exp / expw) as f64)
        .collect_vec()
}
