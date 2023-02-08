use crate::grammar::*;
use crate::semantics::*;
use itertools::*;
use rsdd::repr::ddnnf::DDNNFPtr;
use rsdd::repr::var_order::VarOrder;
use tracing::debug;

pub fn wmc_prob(env: &mut Env, c: &Compiled) -> (f32, f32) {
    let (params, mx) = weight_map_to_params(&c.weight_map);
    let var_order = VarOrder::linear_order(mx as usize);
    let a = env.mgr.and(c.dist, c.accept).wmc(&var_order, &params);
    let z = c.accept.wmc(&var_order, &params);
    (a, z)
}
pub fn exact_inf(env: &mut Env, p: Program) -> f32 {
    let c = compile(env, p);
    let (a, z) = wmc_prob(env, &c);
    debug!(a = a, z = z);
    a / z
}
pub fn importance_weighting_inf(env: &mut Env, steps: usize, p: Program) -> f32 {
    let (mut exp, mut expw, mut expw2) = (0.0, 0.0, 0.0);
    let mut ws: Vec<f64> = vec![];
    let mut ps: Vec<f64> = vec![];
    let mut qs: Vec<f64> = vec![];
    for _step in 1..=steps {
        // FIXME: change back to step
        env.reset_names();
        let c = compile(env, p.clone());
        let (a, z) = wmc_prob(env, &c);
        let pr = (a / z) as f64;
        let q = c.probability.as_f64() as f64;
        let w = c.importance_weight;
        exp = exp + w * q * pr;
        expw = expw + w * q;
        expw2 = expw2 + (q * w * w);
        ws.push(w);
        qs.push(q);
        ps.push(pr);
    }
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    (exp / expw) as f32
}
pub fn importance_weighting_inf_seeded(seeds: Vec<u64>, steps: usize, p: Program) -> f32 {
    let (mut exp, mut expw, mut expw2) = (0.0, 0.0, 0.0);
    let mut ws: Vec<f64> = vec![];
    let mut ps: Vec<f64> = vec![];
    let mut qs: Vec<f64> = vec![];
    let num_seeds = seeds.len();

    for step in 1..=steps {
        let seed = seeds[step % num_seeds];
        let mut envargs = EnvArgs::default_args(Some(seed));
        let mut env = Env::from_args(&mut envargs);
        let c = compile(&mut env, p.clone());
        let (a, z) = wmc_prob(&mut env, &c);
        let pr = (a / z) as f64;
        let q = c.probability.as_f64() as f64;
        let w = c.importance_weight;
        exp = exp + w * q * pr;
        expw = expw + w * q;
        expw2 = expw2 + (q * w * w);
        ws.push(w);
        qs.push(q);
        ps.push(pr);
    }
    // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
    if steps < 11 {
        debug!("");
        debug!(
            "ws = [{}]",
            ws.iter().map(|x| format!("{:.2}", x)).join(", ")
        );
        debug!(
            "qs = [{}]",
            qs.iter().map(|x| format!("{:.2}", x)).join(", ")
        );
        debug!(
            "ps = [{}]",
            ps.iter().map(|x| format!("{:.2}", x)).join(", ")
        );
        debug!("");
        let num = izip!(&ws, &qs, &ps)
            .map(|(w, q, p)| format!("{:.2}*{:.2}*{:.2}", w, q, p))
            .join(" + ");
        let denom = izip!(&ws, &qs)
            .map(|(w, q)| format!("{:.2}*{:.2}", w, q))
            .join(" + ");

        debug!("{}", num);
        debug!("{} = {:.4}", "-".repeat(num.len()), (exp / expw));
        debug!("{}{}", " ".repeat((num.len() - denom.len()) / 2), denom);
    }
    (exp / expw) as f32
}
