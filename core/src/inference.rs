use crate::annotate::grammar::*;
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
use rsdd::repr::wmc::{RealSemiring, WmcParams};
use rsdd::sample::probability::Probability;
use std::cmp;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Sum;
use std::time::Duration;
use std::time::Instant;
use tracing::debug;

#[derive(Debug, Clone, Copy)]
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
    fn empty() -> WmcStats {
        WmcStats {
            dist: 0,
            accept: 0,
            dist_accept: 0,
            mgr_recursive_calls: 0,
        }
    }
}
pub fn calculate_wmc_prob_h(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
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

    let RealSemiring(a) = num.wmc(var_order, params);
    let RealSemiring(z) = accept.wmc(var_order, params);
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
    params: &WmcParams<RealSemiring>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
) -> (f64, WmcStats) {
    let (a, z, stats) = calculate_wmc_prob_h(mgr, params, var_order, dist, accept);
    if a == 0.0 {
        (0.0, stats)
    } else {
        (a / z, stats)
    }
}

pub fn calculate_wmc_prob(
    mgr: &mut Mgr,
    params: &WmcParams<RealSemiring>,
    var_order: &VarOrder,
    dist: BddPtr,
    accept: BddPtr,
    samples: BddPtr,
) -> (f64, WmcStats) {
    let span = tracing::span!(tracing::Level::DEBUG, "calculate_wmc_prob");
    let _enter = span.enter();
    let accept = mgr.and(samples, accept);
    calculate_wmc_prob_hf64(mgr, params, var_order, dist, accept)
}

// pub fn removable_ids(
//     samples_opt: &HashMap<BddPtr, (Option<DecoratedVar>, bool)>,
//     inv: &HashMap<NamedVar, HashSet<BddVar>>,
//     above_below: &HashMap<Var, (HashSet<Var>, HashSet<Var>)>,
// ) -> HashSet<UniqueId> {
//     let mut removable = HashSet::new();
//     for (bdd, (odv, sample)) in samples_opt.clone() {
//         debug!(
//             "checking: {:?}, {:?} == {}",
//             bdd,
//             odv.as_ref().map(DecoratedVar::debug_id),
//             sample
//         );
//         match odv {
//             None => continue,
//             Some(dv) => match dv.var() {
//                 Var::Named(ref nvar) => match above_below.get(&dv.var()) {
//                     None => {
//                         // above_below might be empty if we are not running optimizations
//                         debug!("{:?}", above_below);
//                         panic!("missing {:?}", dv.var())
//                     }
//                     Some((above, below)) => {
//                         debug!("");
//                         debug!("above: {:?}", above.iter().map(Var::id).collect_vec());
//                         debug!("");
//                         debug!("below: {:?}", below.iter().map(Var::id).collect_vec());
//                         debug!("");

//                         // removable.insert(dv.id());
//                         // let xs: HashSet<UniqueId> = rs.iter().map(BddVar::id).collect();
//                         // removable.extend(xs);
//                         let diff: HashSet<_> = above.difference(&below).map(Var::id).collect();
//                         debug!("difference: {:?}", diff);
//                         removable.extend(diff);
//                     }
//                 },
//                 Var::Bdd(bvar) => {
//                     panic!("pretty sure all vars in removable_ids will be namedvars");
//                 }
//             },
//         }
//     }
//     removable
// }

// pub fn included_samples(
//     samples_opt: &HashMap<BddPtr, (Option<DecoratedVar>, bool)>,
//     inv: &HashMap<NamedVar, HashSet<BddVar>>,
//     above_below: &HashMap<Var, (HashSet<Var>, HashSet<Var>)>,
// ) -> HashMap<BddPtr, bool> {
//     let removable = removable_ids(samples_opt, inv, above_below);
//     debug!("removable   {:?}", removable);
//     debug!(
//         "above_below_keys {:?}",
//         above_below.keys().map(Var::id).collect_vec()
//     );

//     samples_opt
//         .iter()
//         .filter(|(bdd, (odv, sampled_value))| match odv {
//             None => true, // keep: this is a top level bdd
//             Some(var) => {
//                 debug!(
//                     "remove {:?}=={:?}? {}",
//                     var,
//                     sampled_value,
//                     removable.contains(&var.id())
//                 );
//                 !removable.contains(&var.id())
//             }
//         })
//         .map(|(k, (_, v))| (*k, *v))
//         .collect()
// }

// pub fn calculate_wmc_prob_opt(
//     mgr: &mut Mgr,
//     params: &WmcParams<RealSemiring>,
//     var_order: &VarOrder,
//     dist: BddPtr,
//     accept: BddPtr,
//     samples_opt: &HashMap<BddPtr, (Option<DecoratedVar>, bool)>,
//     inv: &HashMap<NamedVar, HashSet<BddVar>>,
//     above_below: &HashMap<Var, (HashSet<Var>, HashSet<Var>)>,
// ) -> (f64, WmcStats) {
//     let span = tracing::span!(tracing::Level::DEBUG, "calculate_wmc_prob_opt");
//     let _enter = span.enter();

//     let samples = included_samples(samples_opt, inv, above_below);
//     debug!("samples_opt {:?}", samples_opt);
//     debug!("samples     {:?}", samples);

//     let compiled_samples = samples
//         .iter()
//         .fold(BddPtr::PtrTrue, |formula, (bdd, sampled_value)| {
//             let dist_holds = mgr.iff(*bdd, BddPtr::from_bool(*sampled_value));
//             mgr.and(formula, dist_holds)
//         });
//     debug!("compiled_samples {:?}", compiled_samples.print_bdd());
//     let accept = mgr.and(compiled_samples, accept);
//     calculate_wmc_prob_hf64(mgr, params, var_order, dist, accept)
// }

#[cfg(test)]
mod tests {
    use crate::annotate::grammar::Var;
    use crate::inference::*;
    use crate::*;
    use std::collections::HashMap;
    use std::collections::HashSet;
    use tracing_test::*;
    // #[test]
    // #[traced_test]
    // pub fn test_wmc_opt() {
    //     // let mut mgr = Mgr::new_default_order(2 as usize);
    //     // let var_order = mgr.get_order().clone();
    //     // let mut params = WmcParams::new(0.0, 1.0);
    //     // let (lbl, ptr) = mgr.new_var(true);
    //     // for (lbl, weight) in [(lbl, Weight::new(0.7500, 0.2500))] {
    //     //     params.set_weight(lbl, weight.lo, weight.hi);
    //     // }
    //     // let var = Var {
    //     //     id: UniqueId(0),
    //     //     label: None,
    //     //     provenance: Some("x".to_string()),
    //     // };
    //     // let flip = Var {
    //     //     id: UniqueId(1),
    //     //     label: Some(lbl),
    //     //     provenance: None,
    //     // };

    //     // let dv_var = DecoratedVar {
    //     //     var: var.clone(),
    //     //     above: HashSet::from([flip.clone()]),
    //     //     below: HashSet::from([]),
    //     // };
    //     // let sampleb = true;
    //     // let sample = BddPtr::from_bool(sampleb);

    //     // let dv_flip = DecoratedVar {
    //     //     var: flip.clone(),
    //     //     above: HashSet::from([var.clone()]),
    //     //     below: HashSet::from([]),
    //     // };

    //     // let all_dv = HashMap::from([
    //     //     (var.clone(), dv_var.clone()),
    //     //     (flip.clone(), dv_flip.clone()),
    //     // ]);

    //     // let substitutions = HashMap::from([(UniqueId(0), ([sample], var.clone()))]);
    //     // let samples_opt = HashMap::from([(ptr, (Some(dv_var), sampleb))]);

    //     let prog = Program::Body(lets![
    //        "x" ; B!() ;= sample!(flip!(1/4));
    //        ...? var!("x") ; B!()
    //     ]);
    //     let mut mgr = crate::make_mgr(&prog);
    //     let opt = crate::Options {
    //         opt: true,
    //         debug: false,
    //         seed: Some(4),
    //     };

    //     let p = typecheck(&prog).unwrap();
    //     let mut senv = SymEnv::default();
    //     let p = senv.uniquify(&p).unwrap();
    //     let mut lenv = LabelEnv::new();
    //     let (p, vo, varmap, inv, mxlbl) = lenv.annotate(&p).unwrap();

    //     let mut aenv = AnalysisEnv::new(&varmap);
    //     let (p, sis) = aenv.decorate(&p, opt.opt).unwrap();
    //     let mut rng = opt.rng();
    //     let orng = if opt.debug { None } else { Some(&mut rng) };
    //     let env = Env::new(&mut mgr, orng, opt.opt, inv, sis.clone()); // technically don't need this if I use the decorated vars in a clever way
    //     debug!("{:?}", env.inv);

    //     let mut mgr = Mgr::new_default_order(2 as usize);
    //     let var_order = mgr.get_order().clone();
    //     let mut params = WmcParams::new(RealSemiring(0.0), RealSemiring(1.0));
    //     let (lbl, ptr) = mgr.new_var(true);
    //     for (lbl, weight) in [(lbl, Weight::new(0.7500, 0.2500))] {
    //         params.set_weight(lbl, weight.lo, weight.hi);
    //     }
    //     let nvar = NamedVar {
    //         id: UniqueId(0),
    //         name: "x".to_string(),
    //     };
    //     let var = Var::Named(nvar.clone());
    //     let bvar = BddVar {
    //         id: UniqueId(1),
    //         label: lbl,
    //         provenance: Some(nvar.clone()),
    //     };
    //     let flip = Var::new_bdd(UniqueId(1), lbl, Some(nvar.clone()));

    //     let dv_var = DecoratedVar::new(&var, HashSet::from([flip.clone()]), HashSet::from([]));
    //     let sampleb = false;
    //     let sample = BddPtr::from_bool(sampleb);

    //     let dv_flip = DecoratedVar::new(&flip, HashSet::from([var.clone()]), HashSet::from([]));
    //     let all_dv = HashMap::from([
    //         (var.clone(), dv_var.clone()),
    //         (flip.clone(), dv_flip.clone()),
    //     ]);

    //     // unnecessary, but exactly what the substitutions are
    //     let substitutions = HashMap::from([(UniqueId(0), ([sample], var.clone()))]);
    //     // sample distributions to their named let-bindings (and value)
    //     let samples_opt = HashMap::from([(ptr, (Some(dv_var.clone()), sampleb))]);
    //     // named let-bindings and all associated bdd pointers
    //     let inv = HashMap::from([(nvar.clone(), HashSet::from([bvar.clone()]))]);
    //     debug!("{:?}", inv);
    //     debug!("{:?}", &dv_var);
    //     // debug!("{:?}", inv.get(&dv_var.var));

    //     let removable = removable_ids(&samples_opt, &inv, &sis);
    //     debug!("{:?}", removable);

    //     for (bdd, (odv, sample)) in samples_opt.clone() {
    //         let include = match &odv {
    //             None => false,
    //             Some(dv) => removable.contains(&dv.id()),
    //         };
    //         debug!("remove {:?}? {}", odv.unwrap().var(), include);
    //     }
    //     let w = calculate_wmc_prob_opt(
    //         &mut mgr,
    //         &params,
    //         &var_order,
    //         sample,
    //         BddPtr::PtrTrue,
    //         &samples_opt,
    //         &inv,
    //         &sis,
    //     )
    //     .0;

    //     if sampleb {
    //         assert_eq!(w, 1.0);
    //     } else {
    //         assert_eq!(w, 0.0);
    //     }
    // }
}

pub fn wmc_prob(mgr: &mut Mgr, c: &Output) -> (Vec<f64>, WmcStats) {
    let span = tracing::span!(tracing::Level::DEBUG, "wmc_prob");
    let _enter = span.enter();
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

// pub fn wmc_prob_opt(
//     mgr: &mut Mgr,
//     c: &Output,
//     inv: &HashMap<NamedVar, HashSet<BddVar>>,
//     above_below: &HashMap<Var, (HashSet<Var>, HashSet<Var>)>,
// ) -> (Vec<f64>, WmcStats) {
//     let span = tracing::span!(tracing::Level::DEBUG, "wmc_prob_opt");
//     let _enter = span.enter();
//     let mut prev = None;
//     let probs = c
//         .dists
//         .iter()
//         .map(|d| {
//             let (p, stats) = calculate_wmc_prob_opt(
//                 mgr,
//                 &c.weightmap.as_params(mgr.get_order().num_vars() as u64),
//                 &mgr.get_order().clone(),
//                 *d,
//                 c.accept,
//                 &c.samples_opt,
//                 inv,
//                 above_below,
//             );
//             prev = prev
//                 .as_ref()
//                 .map(|prv: &WmcStats| prv.largest_of(&stats))
//                 .or_else(|| Some(stats));
//             p
//         })
//         .collect_vec();
//     (probs, prev.expect("must include at least one output"))
// }

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
pub fn exact_with(p: &ProgramInferable) -> (Vec<f64>, WmcStats) {
    let p = p.strip_samples();
    match crate::run(&p) {
        Ok((mut mgr, c)) => wmc_prob(&mut mgr, &c),
        Err(e) => panic!(
            "\nCompiler Error!!!\n==============\n{}\n==============\n",
            e
        ),
    }
}

pub fn exact(p: &ProgramInferable) -> Vec<f64> {
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
        match crate::runner_h(p, &mut mgr, opt) {
            Ok((cs, inv)) => {
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

pub struct SamplingIter {
    pub current_step: usize,
    pub current_exp: Expectations,
    pub max_steps: usize,
    pub opt: crate::Options, // can only be 1
    pub start: Instant,
    pub program: ProgramInferable,
    pub manager: Mgr,
    pub max_stats: WmcStats,
}
impl SamplingIter {
    pub fn new(steps: usize, p: &ProgramInferable, opt: &crate::Options) -> Self {
        let mgr = crate::make_mgr(p);
        Self {
            current_step: 0,
            current_exp: Expectations::empty(),
            max_steps: steps,
            opt: opt.clone(),
            start: Instant::now(),
            program: p.clone(),
            manager: mgr,
            max_stats: WmcStats::empty(),
        }
    }
}
pub struct SamplingResult {
    pub step: usize,
    pub stats: WmcStats,
    pub expectations: Expectations,
    pub duration: Duration,
    pub weight: Importance,
}

impl Iterator for SamplingIter {
    type Item = SamplingResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_step > self.max_steps {
            return None;
        }
        match crate::runner_h(&self.program, &mut self.manager, &self.opt) {
            Ok((cs, inv)) => {
                let c = cs.as_output()?;
                let step = self.current_step;

                self.current_step += 1;
                let (query_result, stats) = wmc_prob(&mut self.manager, &c);
                self.max_stats = self.max_stats.largest_of(&stats);
                let expectations = Expectations::new(c.importance.clone(), query_result.clone());
                self.current_exp.mut_add(&expectations);
                let stop = Instant::now();
                let duration = stop.duration_since(self.start);
                Some(SamplingResult {
                    step,
                    stats: self.max_stats.clone(),
                    expectations: self.current_exp.clone(),
                    duration,
                    weight: c.importance,
                })
            }
            Err(e) => None,
        }
    }
}

// pub fn importance_weighting_h_h(
//     steps: usize,
//     checkpoints: usize,
//     p: &ProgramInferable,
//     opt: &crate::Options,
// ) -> SamplingResult {
//     let mut expectations = Expectations::empty();
//     let mut weights: Vec<Importance> = vec![];
//     let mut queries: Vec<Vec<f64>> = vec![];
//     let mut mgr = crate::make_mgr(p);
//     let mut stats_max = WmcStats::empty();
//     let mut ckpts = vec![];

//     debug!("running with options: {:#?}", opt);

//     let start = Instant::now();
//     for step in 1..=steps {
//         match crate::runner_h(p, &mut mgr, opt) {
//             Ok((cs, inv)) => {
//                 cs.into_iter().for_each(|c| {
//                     let (query_result, stats) = wmc_prob(&mut mgr, &c);

//                     expectations.mut_add(&Expectations::new(
//                         c.importance.clone(),
//                         query_result.clone(),
//                     ));
//                     weights.push(c.importance.clone());
//                     queries.push(query_result);
//                     stats_max = stats_max.largest_of(&stats);
//                     if step % checkpoints == 0 || step == steps {
//                         let stop = Instant::now();
//                         let duration = stop.duration_since(start);
//                         ckpts.push((step, stats_max.clone(), expectations.clone(), duration));
//                     }
//                 });
//             }
//             Err(e) => panic!("{:?}", e),
//         }
//     }

//     debug!("{}", expectations.to_str());
//     debug!(
//         "ws = {}",
//         weights.iter().map(Importance::weight).sum::<f64>()
//     );
//     // let var := (ws.zip qs).foldl (fun s (w,q) => s + q * (w - ew) ^ 2) 0
//     let x = expectations.compute_query();
//     (ckpts, weights.clone())
// }

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
    pub expw2: Vec<f64>,
    pub cached_query: Option<Vec<f64>>,
}
impl Expectations {
    pub fn empty() -> Self {
        Self {
            exp: vec![],
            expw: vec![],
            expw2: vec![],
            cached_query: None,
        }
    }
    pub fn new(weight: Importance, prs: Vec<f64>) -> Self {
        let (exp, expw, expw2) = prs.into_iter().fold(
            (vec![], vec![], vec![]),
            |(mut exp, mut expw, mut expw2), pr| {
                let w = weight.weight();
                exp.push(w * pr);
                expw.push(w);
                expw2.push(w * w);
                (exp, expw, expw2)
            },
        );
        Self {
            exp,
            expw,
            expw2,
            cached_query: None,
        }
    }
    pub fn add(l: Self, o: Self) -> Self {
        let mut x = l.clone();
        x.mut_add(&o);
        x
    }

    pub fn mut_add(&mut self, o: &Self) {
        if self.exp.is_empty() {
            self.exp = o.exp.clone();
            self.expw = o.expw.clone();
            self.expw2 = o.expw2.clone();
            self.cached_query = o.cached_query.clone();
        } else {
            self.exp = izip!(&self.exp, &o.exp).map(|(l, r)| l + r).collect_vec();
            self.expw = izip!(&self.expw, &o.expw).map(|(l, r)| l + r).collect_vec();
            self.cached_query = None;
        }
    }

    pub fn to_str(&self) -> String {
        let mut s = String::new();
        s.push_str(&String::from("exp : "));
        s.push_str(&renderfloats(&self.exp, false));
        s.push('\n');
        s.push_str(&String::from("expw: "));
        s.push_str(&renderfloats(&self.expw, false));
        s.push('\n');
        s
    }
    pub fn query(&self) -> Vec<f64> {
        izip!(&self.exp, &self.expw)
            .map(|(exp, expw)| if exp == &0.0 { 0.0 } else { exp / expw })
            .collect_vec()
    }
    pub fn compute_query(&mut self) -> Vec<f64> {
        let qs = self.query();
        self.cached_query = Some(qs.clone());
        qs
    }
}

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
//         .map(|(exp, expw)
