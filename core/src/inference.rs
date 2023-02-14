use crate::grammar::*;
use crate::semantics::*;
use itertools::*;
use rsdd::repr::ddnnf::DDNNFPtr;
use rsdd::repr::var_order::VarOrder;
use std::collections::HashMap;
use tracing::debug;

pub fn _wmc_prob(env: &mut Env, m: &WeightMap, f: &Formulas) -> (f64, f64) {
    let (params, mx) = weight_map_to_params(m);
    let var_order = VarOrder::linear_order(mx as usize);
    let a = env.mgr.and(f.dist, f.accept).wmc(&var_order, &params);
    let z = f.accept.wmc(&var_order, &params);
    (a, z)
}
pub fn wmc_prob(env: &mut Env, c: &Compiled) -> Vec<(f64, f64)> {
    c.formulas
        .iter()
        .map(|fs| _wmc_prob(env, &c.weight_map, fs))
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
        Err(e) => panic!("{:?}", e),
    }
}
fn debug_importance_weighting(
    high_precision: bool,
    steps: usize,
    ws: &Vec<f64>,
    qs: &Vec<f64>,
    ps: &Vec<Vec<f64>>,
    ss: &Vec<HashMap<UniqueId, Vec<bool>>>,
    exp: &Vec<f64>,
    expw: &Vec<f64>,
) {
    let fmt_f64 = |x| {
        if high_precision {
            format!("{}", x)
        } else {
            format!("{:.2}", x)
        }
    };
    if steps < 1001 {
        debug!("");
        debug!("ws = [{}]", ws.iter().map(fmt_f64).join(", "));
        debug!("qs = [{}]", qs.iter().map(fmt_f64).join(", "));
        ps.iter().enumerate().for_each(|(i, ps)| {
            debug!("ps[{}] = [{}]", i, ps.iter().map(fmt_f64).join(", "));
        });
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
    let mut qs: Vec<f64> = vec![];
    let mut ps: Vec<Vec<f64>> = vec![];
    let mut ss: Vec<HashMap<UniqueId, Vec<bool>>> = vec![];
    for _step in 1..=steps {
        // FIXME: change back to step
        env.reset_names();
        match compile(env, p) {
            Ok(c) => {
                let azs = wmc_prob(env, &c);
                let prs = azs
                    .into_iter()
                    .map(|(a, z)| {
                        debug!(a = a, z = z);
                        (a / z) as f64
                    })
                    .collect_vec();
                let _q = c.probability.as_f64() as f64;
                let w = c.importance_weight;

                if exp.len() == 0 {
                    exp = vec![0.0; prs.len()];
                    expw = vec![0.0; prs.len()];
                    expw2 = vec![0.0; prs.len()];
                }
                prs.iter().enumerate().for_each(|(i, pr)| {
                    exp[i] = exp[i] + w * pr;
                    expw[i] = expw[i] + w;
                    expw2[i] = expw2[i] + (w * w);
                });
                ws.push(w);
                qs.push(_q);
                ps.push(prs);
                ss.push(env.samples.clone());
            }
            Err(e) => panic!("{:?}", e),
        }
    }
    debug_importance_weighting(true, steps, &ws, &qs, &ps, &ss, &exp, &expw);
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    izip!(exp, expw)
        .map(|(exp, expw)| (exp / expw) as f64)
        .collect_vec()
}

pub fn importance_weighting_inf_seeded(seeds: Vec<u64>, steps: usize, p: &Program) -> Vec<f64> {
    let (mut exp, mut expw, mut expw2) = (vec![], vec![], vec![]);
    let mut ws: Vec<f64> = vec![];
    let mut qs: Vec<f64> = vec![];
    let mut ps: Vec<Vec<f64>> = vec![];
    let mut ss: Vec<HashMap<UniqueId, Vec<bool>>> = vec![];
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
                let _q = c.probability.as_f64() as f64;
                let w = c.importance_weight;

                if exp.len() == 0 {
                    exp = vec![0.0; prs.len()];
                    expw = vec![0.0; prs.len()];
                    expw2 = vec![0.0; prs.len()];
                }
                prs.iter().enumerate().for_each(|(i, pr)| {
                    exp[i] = exp[i] + w * pr;
                    expw[i] = expw[i] + w;
                    expw2[i] = expw2[i] + (w * w);
                });
                ws.push(w);
                qs.push(_q);
                ps.push(prs);
                ss.push(env.samples.clone());
            }
            Err(e) => panic!("{:?}", e),
        }
    }
    debug_importance_weighting(true, steps, &ws, &qs, &ps, &ss, &exp, &expw);
    izip!(exp, expw)
        .map(|(exp, expw)| (exp / expw) as f64)
        .collect_vec()
}
