use crate::annotate::grammar::*;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::utils::render::*;
use crate::*;
use itertools::*;
use num_traits::*;
use rand::distributions::Distribution;
use rand::rngs::StdRng;
use rand::SeedableRng;
use rsdd::builder::bdd_plan::BddPlan;
use rsdd::builder::cache::all_app::AllTable;
use rsdd::builder::cache::*;
use rsdd::repr::bdd::*;
use rsdd::repr::ddnnf::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::*;
use rsdd::sample::probability::Probability;
use statrs::distribution::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::string::String;
use tracing::*;

pub use crate::data::errors::{CompileError, Result};
// pub use crate::data::importance::{Importance, I};
pub use crate::data::output::{Output, SubstMap};
pub use crate::data::{Weight, WeightMap};
use CompileError::*;

use super::anf::*;
use super::grammar::*;

pub const SQRT_2PI: f64 = 2.5066282746310005024157652848110452530069867406099;

// pub fn eval_eobserve<'a>(
//     mgr: &'a mut Mgr,
//     ctx: &'a Ctx,
//     a: &'a Anf<Annotated, EVal>,
//     opts: &'a Opts,
// ) -> Result<(
//     Output,
//     AnfTr<EVal>,
//     f64,
//     &'a dyn Fn(Output, AnfTr<EVal>) -> EExprTr,
// )> {
//     let (comp, atr, _) = eval_eanf(mgr, ctx, a)?;

//     debug!("In. Accept {}", &ctx.accept.print_bdd());
//     debug!("Comp. dist {}", renderbdds(&comp.dists));
//     debug!("weightmap  {:?}", ctx.weightmap);
//     let dist = comp
//         .dists
//         .into_iter()
//         .fold(ctx.accept, |global, cur| mgr.and(global, cur));
//     let var_order = opts.order.clone();
//     let wmc_params = ctx.weightmap.as_params(opts.max_label);
//     let avars = crate::utils::variables(dist);
//     for (i, var) in avars.iter().enumerate() {
//         debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
//     }
//     debug!("WMCParams  {:?}", wmc_params);
//     debug!("VarOrder   {:?}", var_order);
//     debug!("Accept     {}", dist.print_bdd());
//     // FIXME: should be aggregating these stats somewhere
//     debug!("using optimizations: {}", opts.sample_pruning);

//     let (wmc, _) = crate::inference::calculate_wmc_prob(
//         mgr,
//         &wmc_params,
//         &var_order,
//         dist,
//         ctx.accept,
//         if opts.sample_pruning {
//             BddPtr::PtrTrue
//         } else {
//             ctx.samples
//         },
//     );

//     // let importance = I::Weight(wmc);
//     // debug!("IWeight    {}", importance.weight());

//     let o = Output {
//         dists: vec![BddPtr::PtrTrue],
//         accept: dist,
//         samples: ctx.samples,
//         weightmap: ctx.weightmap.clone(),
//         substitutions: ctx.substitutions.clone(),
//         probabilities: vec![Probability::new(1.0)],
//         // importance,
//         ssubstitutions: ctx.ssubstitutions.clone(),
//         sout: vec![],
//     };
//     Ok((o, atr, wmc, &move |c, atr| {
//         EExpr::EObserve(Box::new(c), Box::new(atr))
//     }))
// }

pub fn eval_eite_predicate(
    mgr: &mut Mgr,
    ctx: &Ctx,
    cond: &AnfAnn<EVal>,
    opts: &Opts,
) -> Result<(BddPlan, AnfTr<EVal>, (Probability, Probability))> {
    let (pred, atr) = eval_eanf(&ctx.exact, cond)?;
    let pred_dist = pred.dists();
    if !pred_dist.len() == 1 {
        return Err(TypeError(format!(
            "Expected EBool for ITE condition\nGot: {cond:?}\n{ctx:?}",
        )));
    }
    let pred_dist = pred_dist[0].clone();
    let var_order = opts.order.clone();
    let wmc_params = ctx.exact.weightmap.as_params(opts.max_label);

    // FIXME : should be adding stats somewhere
    let mut wmc_opt_h = |pred_dist| {
        Probability::new(
            crate::inference::calculate_wmc_prob(
                mgr,
                &wmc_params,
                &var_order,
                pred_dist,
                ctx.exact.accept.clone(),
                // TODO if switching to samples_opt, no need to use ctx.
                ctx.exact.samples(opts.sample_pruning),
            )
            .0,
        )
    };
    let wmc_true = wmc_opt_h(pred_dist.clone());
    let wmc_false = wmc_opt_h(BddPlan::neg(pred_dist.clone()));
    Ok((pred_dist, atr, (wmc_true, wmc_false)))
}
pub fn eval_eite_output<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Ctx,
    pred: (BddPlan, AnfTr<EVal>),
    truthy_branch: (Probability, Output),
    falsey_branch: (Probability, Output),
    opts: &'a Opts,
) -> Result<EOutput> {
    let (pred_dist, pred_anf) = pred;
    let (wmc_true, truthy) = truthy_branch;
    let (wmc_false, falsey) = falsey_branch;
    let dists = izip!(&truthy.exact.dists(), &falsey.exact.dists())
        .map(|(tdist, fdist)| {
            let dist_l = pred_dist._and(tdist);
            let dist_r = pred_dist._neg()._and(fdist);
            EVal::EBdd(dist_l._or(&dist_r))
        })
        .collect_vec();

    let tsamples = GetSamples::samples(&truthy.exact, opts.sample_pruning);
    let mut samples = vec![];
    if !opts.sample_pruning {
        samples.extend(truthy.exact.samples);
        samples.extend(falsey.exact.samples);
    };

    let accept_l = pred_dist._and(&truthy.exact.accept);
    let accept_r = pred_dist._neg()._and(&falsey.exact.accept);
    let accept = accept_l._or(&accept_r);
    let accept = accept._and(&ctx.exact.accept);

    let mut substitutions = truthy.exact.substitutions.clone();
    substitutions.extend(falsey.exact.substitutions.clone());
    let mut weightmap = truthy.exact.weightmap.clone();
    weightmap.weights.extend(falsey.exact.weightmap.clone());

    // let probabilities = izip!(&truthy.probabilities, &falsey.probabilities)
    //     .map(|(t, f)| (*t * wmc_true + *f * wmc_false))
    //     .collect_vec();

    // debug!("=============================");
    // let importance_true = truthy.importance.pr_mul(wmc_true);
    // let importance_false = falsey.importance.pr_mul(wmc_false);
    // debug!(
    //     "importance_true {:?}, importance_false {:?}",
    //     importance_true, importance_false
    // );
    // let importance = importance_true + importance_false;
    // debug!("importance {:?}", importance);
    // debug!("=============================");

    Ok(EOutput {
        out: dists,
        accept,
        samples,
        weightmap,
        substitutions,
    })
}

pub fn eval_elet_output(
    mgr: &mut Mgr,
    ctx: &Ctx,
    bound: Output,
    body: Output,
    opts: &Opts,
) -> Result<EOutput> {
    let accept = body.exact.accept._and(&ctx.exact.accept);
    let mut samples = ctx.exact.samples.clone();
    samples.extend(body.exact.samples);

    // let probabilities = izip!(bound.probabilities.clone(), body.probabilities)
    //     .map(|(p1, p2)| p1 * p2)
    //     .collect_vec();
    // // let importance = I::Weight(bound.importance.weight() * body.importance.weight());

    let c = EOutput {
        out: body.exact.out,
        accept,
        samples,
        substitutions: body.exact.substitutions.clone(),
        weightmap: body.exact.weightmap,
    };
    Ok(c)
}

// fn sbern(ctx: &Ctx, param: &AnfAnn<SVal>) -> Result<(Bernoulli, AnfTr<SVal>)> {
//     let (cparam, param, _) = crate::compile::anf::eval_sanf(ctx, param)?;

//     let dist = statrs::distribution::Bernoulli::new(cparam.sfloat()).unwrap();
//     Ok((dist, param))
// }
// fn sdiscrete(ctx: &Ctx, ps: &Vec<AnfAnn<SVal>>) -> Result<(Categorical, Vec<AnfTr<SVal>>)> {
//     let (cps, ps) = crate::compile::anf::eval_sanfs(ctx, ps)?;

//     let dist = statrs::distribution::Categorical::new(&cps.sfloats()).unwrap();
//     Ok((dist, ps))
// }
// fn sdirichlet(ctx: &Ctx, ps: &Vec<AnfAnn<SVal>>) -> Result<(Dirichlet, Vec<AnfTr<SVal>>)> {
//     let (cps, ps) = crate::compile::anf::eval_sanfs(ctx, ps)?;

//     let dist = statrs::distribution::Dirichlet::new(cps.sfloats()).unwrap();
//     Ok((dist, ps))
// }
// fn suniform(
//     ctx: &Ctx,
//     lo: &AnfAnn<SVal>,
//     hi: &AnfAnn<SVal>,
// ) -> Result<(Uniform, AnfTr<SVal>, AnfTr<SVal>)> {
//     let (clo, lo, _) = crate::compile::anf::eval_sanf(ctx, lo)?;
//     let (chi, hi, _) = crate::compile::anf::eval_sanf(ctx, hi)?;

//     let dist = statrs::distribution::Uniform::new(clo.sfloat(), chi.sfloat()).unwrap();
//     Ok((dist, lo, hi))
// }
// fn snormal(
//     ctx: &Ctx,
//     mn: &AnfAnn<SVal>,
//     sd: &AnfAnn<SVal>,
// ) -> Result<(Normal, AnfTr<SVal>, AnfTr<SVal>)> {
//     let (cmn, mn, _) = crate::compile::anf::eval_sanf(ctx, mn)?;
//     let (csd, sd, _) = crate::compile::anf::eval_sanf(ctx, sd)?;

//     let dist = statrs::distribution::Normal::new(cmn.sfloat(), csd.sfloat()).unwrap();
//     Ok((dist, mn, sd))
// }
// fn sbeta(
//     ctx: &Ctx,
//     a: &AnfAnn<SVal>,
//     b: &AnfAnn<SVal>,
// ) -> Result<(Beta, AnfTr<SVal>, AnfTr<SVal>)> {
//     let (ca, a, _) = crate::compile::anf::eval_sanf(ctx, a)?;
//     let (cb, b, _) = crate::compile::anf::eval_sanf(ctx, b)?;
//     let dist = statrs::distribution::Beta::new(ca.sfloat(), cb.sfloat()).unwrap();
//     Ok((dist, a, b))
// }

#[derive(Clone, Debug)]
pub struct Opts {
    pub sample_pruning: bool,
    pub max_label: u64,
    pub order: VarOrder,
}

pub enum Fn {
    Exact(Function<EExpr<Annotated>>),
    Sample(Function<SExpr<Annotated>>),
}
impl Fn {
    fn exact(&self) -> Result<Function<EExpr<Annotated>>> {
        match self {
            Self::Exact(f) => Ok(f.clone()),
            Self::Sample(f) => {
                errors::generic("tried to extract a sample function in the exact language")
            }
        }
    }
    fn sample(&self) -> Result<Function<SExpr<Annotated>>> {
        match self {
            Self::Exact(f) => {
                errors::generic("tried to extract an exact function in the sampling language")
            }
            Self::Sample(f) => Ok(f.clone()),
        }
    }
}

pub struct State<'a> {
    pub opts: Opts,
    pub mgr: &'a mut Mgr,
    pub rng: Option<&'a mut StdRng>, // None implies "debug mode"
    pub fns: HashMap<String, Fn>,
    pub pq: PQ,
}

impl<'a> State<'a> {
    pub fn new(
        mgr: &'a mut Mgr,
        rng: Option<&'a mut StdRng>, // None implies "debug mode"
        sample_pruning: bool,
    ) -> State<'a> {
        let opts = Opts {
            order: mgr.get_order().clone(),
            max_label: mgr.get_order().num_vars() as u64,
            sample_pruning,
        };
        State {
            opts,
            mgr,
            rng,
            pq: Default::default(),
            fns: Default::default(),
        }
    }
    pub fn new_from<'b>(&self) -> State<'b> {
        todo!()
        // State {
        //     opts: self.opts.clone(),
        //     mgr,
        //     rng,
        //     pq: Default::default(),
        // }
    }

    pub fn p(&self) -> f64 {
        self.pq.p
    }
    pub fn q(&self) -> f64 {
        self.pq.q
    }
    pub fn eval_program(&mut self, prog: &Program<Annotated>) -> Result<(Output, Program<Trace>)> {
        match prog {
            Program::SBody(e) => {
                let (c, e) = self.eval_sexpr(Ctx::default(), e)?;
                Ok((c, Program::SBody(e)))
            }
            Program::EBody(e) => {
                let (c, e) = self.eval_eexpr(Ctx::default(), e)?;
                Ok((c, Program::EBody(e)))
            }
            Program::SDefine(f, e) => {
                let name = f
                    .clone()
                    .name
                    .expect("all defined functions must have a name");
                self.fns.insert(name, Fn::Sample(f.clone()));
                self.eval_program(e)
            }
            Program::EDefine(f, e) => {
                let name = f
                    .clone()
                    .name
                    .expect("all defined functions must have a name");
                self.fns.insert(name, Fn::Exact(f.clone()));
                self.eval_program(e)
            }
        }
    }

    pub fn eval_eexpr(&mut self, ctx: Ctx, e: &EExprAnn) -> Result<(Output, EExprTr)> {
        use EExpr::*;
        match e {
            EAnf(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "anf");
                let _enter = span.enter();
                let (o, a) = eval_eanf(&ctx.exact, a)?;
                debug_step_ng!("anf", ctx, &o);
                let out = ctx.mk_eoutput(o);
                Ok((out.clone(), EExpr::EAnf(out, Box::new(a))))
            }
            EFlip(d, param) => {
                let span = tracing::span!(tracing::Level::DEBUG, "flip");
                let _enter = span.enter();

                let (o, param) = eval_eanf(&ctx.exact, param)?;
                let o = match &o.out[..] {
                    [EVal::EFloat(param)] => {
                        let mut weightmap = ctx.exact.weightmap.clone();
                        weightmap.insert(d.label, *param);
                        Ok(EOutput {
                            out: vec![EVal::EBdd(BddPlan::Literal(d.label, true))],
                            accept: ctx.exact.accept.clone(),
                            samples: ctx.exact.samples.clone(),
                            weightmap,
                            substitutions: ctx.exact.substitutions.clone(),
                        })
                    }
                    _ => errors::typecheck_failed(),
                }?;

                debug_step_ng!("flip", ctx, o);
                let out = ctx.mk_eoutput(o);
                Ok((out.clone(), EExpr::EFlip(out, Box::new(param))))
            }
            EObserve(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "observe");
                let _enter = span.enter();

                let (comp, a) = eval_eanf(&ctx.exact, a)?;

                // debug!("In. Accept {}", &ctx.accept.print_bdd());
                // debug!("Comp. dist {}", renderbdds(&comp.dists));
                // debug!("weightmap  {:?}", ctx.weightmap);
                let dist = comp
                    .dists()
                    .into_iter()
                    .fold(ctx.exact.accept.clone(), |global, cur| {
                        BddPlan::and(global, cur)
                    });

                let var_order = self.opts.order.clone();
                let wmc_params = ctx.exact.weightmap.as_params(self.opts.max_label);
                let avars = crate::utils::plan_variables(&dist);
                for (i, var) in avars.iter().enumerate() {
                    debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
                }
                debug!("WMCParams  {:?}", wmc_params);
                debug!("VarOrder   {:?}", var_order);
                debug!("Accept     {:?}", &dist);
                // FIXME: should be aggregating these stats somewhere
                debug!("using optimizations: {}", self.opts.sample_pruning);

                let (wmc, _) = crate::inference::calculate_wmc_prob(
                    self.mgr,
                    &wmc_params,
                    &var_order,
                    dist.clone(),
                    ctx.exact.accept.clone(),
                    ctx.exact.samples(self.opts.sample_pruning),
                );

                // let importance = I::Weight(wmc);
                // debug!("IWeight    {}", importance.weight());

                let o = EOutput {
                    out: vec![EVal::EBdd(BddPlan::ConstTrue)],
                    accept: dist.clone(),
                    samples: ctx.exact.samples.clone(),
                    weightmap: ctx.exact.weightmap.clone(),
                    substitutions: ctx.exact.substitutions.clone(),
                };

                // let Importance::Weight(theta) = o.importance;
                self.pq.p *= wmc; // <<<<<<<<<<<<<<<<<<<<

                debug_step_ng!("observe", ctx, o);
                let out = ctx.mk_eoutput(o);
                Ok((out.clone(), EExpr::EObserve(out, Box::new(a))))
            }
            EIte(_, cond, t, f) => {
                let span = tracing::span!(tracing::Level::DEBUG, "ite");
                let _enter = span.enter();
                let (pred_dist, pred_anf, (wmc_true, wmc_false)) =
                    eval_eite_predicate(self.mgr, &ctx, cond, &self.opts)?;

                debug!("=============================");
                debug!("wmc_true {}, wmc_false {}", wmc_true, wmc_false);
                debug!("=============================");

                let tspan = tracing::span!(tracing::Level::DEBUG, "tru");
                let _tenter = tspan.enter();
                let (truthy, ttr) = self.eval_eexpr(ctx.clone(), t)?;
                drop(_tenter);

                let fspan = tracing::span!(tracing::Level::DEBUG, "fls");
                let _fenter = fspan.enter();
                let (falsey, ftr) = self.eval_eexpr(ctx.clone(), f)?;
                drop(_fenter);

                if truthy.exact.out.len() != falsey.exact.out.len() {
                    return Err(TypeError(format!("Expected both branches of ITE to return same len tuple\nGot (left): {:?}\nGot (right):{:?}", truthy.exact.out.len(), falsey.exact.out.len(),)));
                }
                let o: EOutput = eval_eite_output(
                    self.mgr,
                    &ctx,
                    (pred_dist, pred_anf.clone()),
                    (wmc_true, truthy.clone()),
                    (wmc_false, falsey),
                    &self.opts,
                )?;
                debug_step_ng!("ite", ctx, &o);
                let out = ctx.mk_eoutput(o);
                Ok((
                    out.clone(),
                    EIte(out, Box::new(pred_anf), Box::new(ttr), Box::new(ftr)),
                ))
            }
            ELetIn(d, s, ebound, ebody) => {
                let span = tracing::span!(Level::DEBUG, "let", var = s);
                let _enter = span.enter();

                let (bound, eboundtr) = self.eval_eexpr(ctx.clone(), ebound)?;

                // let mut newctx = ctx.new_from_eoutput(&bound);
                let mut newctx = Ctx::from(&bound);
                newctx
                    .exact
                    .substitutions
                    .insert(d.id(), (bound.exact.out.clone(), Var::Named(d.clone())));
                let (body, bodiestr) = self.eval_eexpr(newctx, ebody)?;

                let o = eval_elet_output(self.mgr, &ctx, bound.clone(), body, &self.opts)?;
                debug_step_ng!(format!("let-in {}", s), ctx, o);

                let outs = ctx.mk_eoutput(o);
                Ok((
                    outs.clone(),
                    ELetIn(outs, s.clone(), Box::new(eboundtr), Box::new(bodiestr)),
                ))
            }
            EApp(_, fname, args) => {
                let f = self.fns.get(fname).expect("function is defined").exact()?;
                let params = f.args2vars(|v| match v {
                    Anf::AVar(nv, _) => Ok(nv.clone()),
                    _ => errors::generic(&format!(
                        "function {:?} argument definitions malformed",
                        fname
                    )),
                })?;
                let (argvals, args) = eval_eanfs(&ctx.exact, args)?;
                let argvals = argvals.out;
                if params.len() != argvals.len() {
                    return errors::generic(&format!("function {:?} arguments mismatch", fname));
                }
                let mut subs = ctx.exact.substitutions.clone();
                for (param, val) in params.iter().zip(argvals.iter()) {
                    subs.insert(param.id(), (vec![val.clone()], Var::Named(param.clone())));
                }
                let mut callerctx = ctx.clone();
                callerctx.exact.substitutions = subs;

                let (out, _) = self.eval_eexpr(callerctx, &f.body)?;
                Ok((out.clone(), EApp(out.clone(), fname.clone(), args)))
            }
            EIterate(_, fname, init, k) => {
                let span = tracing::span!(Level::DEBUG, "iterate");
                let _enter = span.enter();
                let (args, init) = eval_eanf(&ctx.exact, init)?;
                let mut args = args.out;
                let (niters, k) = eval_eanf(&ctx.exact, k)?;
                let mut niters: usize = match &niters.out[..] {
                    [EVal::EInteger(i)] => errors::erased(),
                    [EVal::EProd(vs)] => crate::desugar::exact::integers::from_prod(vs),
                    _ => errors::typecheck_failed(),
                }?;
                let mut ctx = ctx.clone();
                let mut out = None;
                while niters > 0 {
                    let anfargs = args.iter().map(|i| Anf::AVal((), i.clone())).collect();
                    let o = self
                        .eval_eexpr(ctx.clone(), &EApp((), fname.clone(), anfargs))?
                        .0;
                    out = Some(o.clone());
                    ctx = ctx.new_from_eoutput(&o.exact);
                    args = o.exact.out;
                    niters -= 1;
                }
                let out = out.expect("k > 0");
                Ok((
                    out.clone(),
                    EIterate(out, fname.clone(), Box::new(init), Box::new(k)),
                ))
            }
            ESample((), sexpr) => {
                let (mut o, s) = self.eval_sexpr(ctx, sexpr)?;
                let out = match o
                    .sample
                    .out
                    .iter()
                    .map(EExpr::<Trace>::embed)
                    .collect::<Option<Vec<_>>>()
                {
                    None => {
                        errors::semantics("natural embedding failed -- check semantics or types")
                    }
                    Some(os) => Ok(os),
                }?;
                o.exact.out = out;
                Ok((o.clone(), ESample(o, Box::new(s))))
            }
            EDiscrete(_, _) => errors::erased(),
        }
    }

    pub fn eval_sexpr(&mut self, ctx: Ctx, e: &SExprAnn) -> Result<(Output, SExprTr)> {
        use SExpr::*;
        todo!()
    }
    //         match e {
    //             SAnf(_, a) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "sanf");
    //                 let _enter = span.enter();
    //                 debug!("anf: {:?}", a);

    //                 let (o, a, mk) = eval_sanf(&ctx, a)?;
    //                 let c = Output::Output(o);
    //                 Ok((c.clone(), mk(c, a)))
    //             }
    //             SBern(_, param) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "bernoulli");
    //                 let _enter = span.enter();

    //                 let (dist, param) = sbern(&ctx, param)?;
    //                 let x = match self.rng.as_mut() {
    //                     Some(rng) => dist.sample(rng),
    //                     None => dist.sample(&mut rand::thread_rng()),
    //                 };
    //                 let x = x != 0.0;
    //                 let weight = statrs::distribution::Discrete::pmf(&dist, x as u64);
    //                 let mut o = Output::for_sample_lang(&ctx);
    //                 self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 debug!(q = self.pq.q, test = "", p = self.pq.p);

    //                 o.sout = vec![SVal::SBool(x)];
    //                 let c = Output::from_output(o);
    //                 Ok((c.clone(), SBern(Box::new(c), Box::new(param))))
    //             }
    //             SDiscrete(_, ps) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "discrete");
    //                 let _enter = span.enter();

    //                 let (dist, ps) = sdiscrete(&ctx, ps)?;
    //                 let x = match self.rng.as_mut() {
    //                     Some(rng) => dist.sample(rng),
    //                     None => dist.sample(&mut rand::thread_rng()),
    //                 };
    //                 let x = x as u64;
    //                 let weight = statrs::distribution::Discrete::pmf(&dist, x); // internally, this sample was cast as f64, so this is okay
    //                 let mut o = Output::for_sample_lang(&ctx);
    //                 self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 o.sout = vec![SVal::SInt(x)];
    //                 let c = Output::from_output(o);
    //                 Ok((c.clone(), SDiscrete(Box::new(c), ps.clone())))
    //             }
    //             SDirichlet(_, ps) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "dirichlet");
    //                 let _enter = span.enter();

    //                 let (dist, ps) = sdirichlet(&ctx, ps)?;
    //                 let x = match self.rng.as_mut() {
    //                     Some(rng) => dist.sample(rng),
    //                     None => dist.sample(&mut rand::thread_rng()),
    //                 };
    //                 let weight = statrs::distribution::Continuous::pdf(&dist, &x);
    //                 let mut o = Output::for_sample_lang(&ctx);
    //                 self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 let x = x.data.as_vec().to_vec();
    //                 o.sout = x.into_iter().map(SVal::SFloat).collect();
    //                 let c = Output::from_output(o);
    //                 Ok((c.clone(), SDirichlet(Box::new(c), ps.clone())))
    //             }
    //             SUniform(_, lo, hi) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "uniform");
    //                 let _enter = span.enter();

    //                 let (dist, lo, hi) = suniform(&ctx, lo, hi)?;
    //                 let x = match self.rng.as_mut() {
    //                     Some(rng) => dist.sample(rng),
    //                     None => dist.sample(&mut rand::thread_rng()),
    //                 };
    //                 let weight = statrs::distribution::Continuous::pdf(&dist, x);
    //                 let mut o = Output::for_sample_lang(&ctx);
    //                 self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 o.sout = vec![SVal::SFloat(x)];
    //                 let c = Output::from_output(o);
    //                 Ok((c.clone(), SUniform(Box::new(c), Box::new(lo), Box::new(hi))))
    //             }
    //             SNormal(_, mn, sd) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "normal");
    //                 let _enter = span.enter();

    //                 let (dist, mn, sd) = snormal(&ctx, mn, sd)?;

    //                 let x = match self.rng.as_mut() {
    //                     Some(rng) => dist.sample(rng),
    //                     None => dist.sample(&mut rand::thread_rng()),
    //                 };

    //                 let weight = statrs::distribution::Continuous::pdf(&dist, x);
    //                 let mut o = Output::for_sample_lang(&ctx);
    //                 self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 o.sout = vec![SVal::SFloat(x)];
    //                 let c = Output::from_output(o);
    //                 Ok((c.clone(), SNormal(Box::new(c), Box::new(mn), Box::new(sd))))
    //             }
    //             SBeta(_, a, b) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "beta");
    //                 let _enter = span.enter();

    //                 let (dist, a, b) = sbeta(&ctx, a, b)?;

    //                 let x = match self.rng.as_mut() {
    //                     Some(rng) => dist.sample(rng),
    //                     None => dist.sample(&mut rand::thread_rng()),
    //                 };
    //                 let weight = statrs::distribution::Continuous::pdf(&dist, x);
    //                 let mut o = Output::for_sample_lang(&ctx);
    //                 self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
    //                 o.sout = vec![SVal::SFloat(x)];
    //                 let c = Output::from_output(o);
    //                 Ok((c.clone(), SBeta(Box::new(c), Box::new(a), Box::new(b))))
    //             }
    //             SLetIn(d, name, bindee, body) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "slet", v = name);
    //                 let _enter = span.enter();
    //                 let (cbindee, ebindee) = self.eval_sexpr(ctx, bindee)?;
    //                 // debug!("{:?}", cbindee);
    //                 debug!("BINDEE:: {:?}", ebindee);
    //                 let vbindee = cbindee.unsafe_output().sval();
    //                 debug!("VALUE:: {:?}", vbindee);

    //                 let mut ctx = Ctx::from_compiled(&cbindee.unsafe_output());
    //                 ctx.ssubstitutions.insert(d.id(), vbindee);
    //                 let (cbody, ebody) = self.eval_sexpr(ctx, body)?;
    //                 debug!("SOMETHING");
    //                 Ok((
    //                     cbody.clone(),
    //                     SSeq(Box::new(cbody), Box::new(ebindee), Box::new(ebody)),
    //                 ))
    //             }
    //             SSeq(_, e0, e1) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "sample-seq");
    //                 let _enter = span.enter();
    //                 let (c0, e0) = self.eval_sexpr(ctx, e0)?;
    //                 let ctx = Ctx::from_compiled(&c0.unsafe_output());
    //                 let (c1, e1) = self.eval_sexpr(ctx, e1)?;
    //                 Ok((c1.clone(), SSeq(Box::new(c1), Box::new(e0), Box::new(e1))))
    //             }
    //             SIte(_, guard, truthy, falsey) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "sample-ite");
    //                 let _enter = span.enter();
    //                 let (cguard, eguard, _) = crate::compile::anf::eval_sanf(&ctx, guard)?;
    //                 if cguard.sbool() {
    //                     self.eval_sexpr(ctx.clone(), truthy)
    //                 } else {
    //                     self.eval_sexpr(ctx, falsey)
    //                 }
    //             }
    //             SObserve(_, a, e) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "sample-observe");
    //                 let _enter = span.enter();

    //                 let (ca, a, _) = crate::compile::anf::eval_sanf(&ctx, a)?;
    //                 let (q, ce, e) = match *e.clone() {
    //                     SBern(_, param) => {
    //                         let (dist, param) = sbern(&ctx, &param)?;

    //                         let q = statrs::distribution::Discrete::pmf(&dist, ca.sbool() as u64);

    //                         let mut o = Output::for_sample_lang(&ctx);
    //                         o.sout = vec![SVal::SBool(ca.sbool())];
    //                         let c = Output::from_output(o);
    //                         (q, c.clone(), SBern(Box::new(c), Box::new(param)))
    //                     }
    //                     SDiscrete(_, ps) => {
    //                         let (dist, ps) = sdiscrete(&ctx, &ps)?;
    //                         let q = statrs::distribution::Discrete::pmf(&dist, ca.sint() as u64);

    //                         let mut o = Output::for_sample_lang(&ctx);
    //                         o.sout = vec![SVal::SInt(ca.sint())];
    //                         let c = Output::from_output(o);
    //                         (q, c.clone(), SDiscrete(Box::new(c), ps.clone()))
    //                     }
    //                     SDirichlet(_, ps) => {
    //                         let (dist, ps) = sdirichlet(&ctx, &ps)?;
    //                         let q = statrs::distribution::Continuous::pdf(
    //                             &dist,
    //                             &<nalgebra::base::DVector<f64> as From<Vec<f64>>>::from(ca.sfloats()),
    //                         );

    //                         let mut o = Output::for_sample_lang(&ctx);
    //                         o.sout = ca.sfloats().into_iter().map(SVal::SFloat).collect();
    //                         let c = Output::from_output(o);
    //                         (q, c.clone(), SDirichlet(Box::new(c), ps.clone()))
    //                     }
    //                     SUniform(_, lo, hi) => {
    //                         let (dist, lo, hi) = suniform(&ctx, &lo, &hi)?;
    //                         let q = statrs::distribution::Continuous::pdf(&dist, ca.sfloat());

    //                         let mut o = Output::for_sample_lang(&ctx);
    //                         o.sout = vec![SVal::SFloat(ca.sfloat())];
    //                         let c = Output::from_output(o);
    //                         (
    //                             q,
    //                             c.clone(),
    //                             SUniform(Box::new(c), Box::new(lo), Box::new(hi)),
    //                         )
    //                     }

    //                     SNormal(_, mn, sd) => {
    //                         let (dist, mn, sd) = snormal(&ctx, &mn, &sd)?;
    //                         let q = statrs::distribution::Continuous::pdf(&dist, ca.sfloat());

    //                         let mut o = Output::for_sample_lang(&ctx);
    //                         o.sout = vec![SVal::SFloat(ca.sfloat())];
    //                         let c = Output::from_output(o);
    //                         (
    //                             q,
    //                             c.clone(),
    //                             SNormal(Box::new(c), Box::new(mn), Box::new(sd)),
    //                         )
    //                     }
    //                     SBeta(_, a, b) => {
    //                         let (dist, a, b) = sbeta(&ctx, &a, &b)?;
    //                         let q = statrs::distribution::Continuous::pdf(&dist, ca.sfloat());

    //                         let mut o = Output::for_sample_lang(&ctx);
    //                         o.sout = vec![SVal::SFloat(ca.sfloat())];
    //                         let c = Output::from_output(o);
    //                         (q, c.clone(), SBeta(Box::new(c), Box::new(a), Box::new(b)))
    //                     }
    //                     _ => panic!("semantic error, see note on SObserve in SExpr enum"),
    //                 };

    //                 self.pq.q *= q;
    //                 let c = Output::from_output(ca);
    //                 Ok((c.clone(), SObserve(Box::new(c), Box::new(a), Box::new(e))))
    //             }

    //             SExact(_, eexpr) => {
    //                 let span = tracing::span!(tracing::Level::DEBUG, "exact");
    //                 let _enter = span.enter();

    //                 let (comp, etr) = self.eval_eexpr(ctx.clone(), eexpr)?;
    //                 let comp: Output = comp.unsafe_output();

    //                 let wmc_params = comp.weightmap.as_params(self.opts.max_label);
    //                 let var_order = self.opts.order.clone();
    //                 debug!("Incm accept {}", &ctx.accept.print_bdd());
    //                 debug!("Comp accept {}", comp.accept.print_bdd());
    //                 debug!("Comp distrb {}", renderbdds(&comp.dists));

    //                 debug!("weight_map {:?}", &comp.weightmap);
    //                 debug!("WMCParams  {:?}", wmc_params);
    //                 debug!("VarOrder   {:?}", var_order);
    //                 let accept = comp.accept;

    //                 let mut samples = comp.samples;
    //                 let (mut qs, mut dists) = (vec![], vec![]);
    //                 let mut bool_samples = vec![];
    //                 for dist in comp.dists.iter() {
    //                     let theta_q;
    //                     // FIXME: should be aggregating the stats somewhere
    //                     debug!("using optimizations: {}", self.opts.sample_pruning);
    //                     if self.opts.sample_pruning {
    //                         (theta_q, _) = crate::inference::calculate_wmc_prob(
    //                             self.mgr,
    //                             &wmc_params,
    //                             &var_order,
    //                             *dist, // TODO switch from *dist
    //                             accept,
    //                             BddPtr::PtrTrue, // TODO &samples
    //                         );
    //                     } else {
    //                         let sample_dist = self.mgr.and(samples, *dist);
    //                         (theta_q, _) = crate::inference::calculate_wmc_prob(
    //                             self.mgr,
    //                             &wmc_params,
    //                             &var_order,
    //                             sample_dist, // TODO switch from *dist
    //                             accept,
    //                             samples, // TODO &samples
    //                         );
    //                     }

    //                     let sample = match self.rng.as_mut() {
    //                         Some(rng) => {
    //                             let bern = rand::distributions::Bernoulli::new(theta_q).unwrap();
    //                             bern.sample(rng)
    //                         }
    //                         None => panic!("full enumeration for debugging is not currently supported"),
    //                     };
    //                     qs.push(Probability::new(if sample {
    //                         theta_q
    //                     } else {
    //                         1.0 - theta_q
    //                     }));
    //                     self.pq.q *= if sample { theta_q } else { 1.0 - theta_q };
    //                     self.pq.p *= if sample { theta_q } else { 1.0 - theta_q };
    //                     bool_samples.push(sample);
    //                     dists.push(BddPtr::from_bool(sample));

    //                     if !self.opts.sample_pruning {
    //                         // sample in sequence. A smarter sample would compile
    //                         // all samples of a multi-rooted BDD, but I need to futz
    //                         // with rsdd's fold
    //                         let dist_holds = self.mgr.iff(*dist, BddPtr::from_bool(sample));
    //                         samples = self.mgr.and(samples, dist_holds);
    //                     }
    //                 }
    //                 debug!("final dists:   {}", renderbdds(&dists));
    //                 debug!("final samples: {}", samples.print_bdd());
    //                 debug!("using optimizations: {}", self.opts.sample_pruning);
    //                 let c = Output {
    //                     dists,
    //                     accept,
    //                     samples,
    //                     weightmap: comp.weightmap.clone(),
    //                     // any dangling references will be treated as
    //                     // constant and, thus, ignored -- information
    //                     // about this will live only in the propagated
    //                     // weight
    //                     substitutions: comp.substitutions.clone(),
    //                     probabilities: qs,
    //                     // importance: I::Weight(1.0),
    //                     ssubstitutions: ctx.ssubstitutions.clone(),
    //                     sout: SVal::from_bools(&bool_samples),
    //                 };
    //                 debug_step!("sample", ctx, c);
    //                 let c = Output::from_output(c);
    //                 Ok((c.clone(), SExact(Box::new(c), Box::new(etr))))
    //             } // sexact supporting debug mode (full enumeration over all samples from an exact program)
    //               // SExact(_, eexpr) => {
    //               //     let span = tracing::span!(tracing::Level::DEBUG, "exact");
    //               //     let _enter = span.enter();

    //               //     let (comp, etr) = self.eval_eexpr(ctx.clone(), eexpr)?;
    //               //     let c: Output = comp
    //               //         .into_iter()
    //               //         .enumerate()
    //               //         .map(|(ix, comp)| {
    //               //             let span = ixspan!("", ix);
    //               //             let _enter = span.enter();

    //               //             let wmc_params = comp.weightmap.as_params(self.opts.max_label);
    //               //             let var_order = self.opts.order.clone();
    //               //             debug!("Incm accept {}", &ctx.accept.print_bdd());
    //               //             debug!("Comp accept {}", comp.accept.print_bdd());
    //               //             debug!("Comp distrb {}", renderbdds(&comp.dists));

    //               //             debug!("weight_map {:?}", &comp.weightmap);
    //               //             debug!("WMCParams  {:?}", wmc_params);
    //               //             debug!("VarOrder   {:?}", var_order);
    //               //             let accept = comp.accept;

    //               //             let mut fin = vec![];

    //               //             for sample_det in [true, false] {
    //               //                 let span = if sample_det {
    //               //                     tracing::span!(tracing::Level::DEBUG, "")
    //               //                 } else {
    //               //                     let s = false;
    //               //                     tracing::span!(tracing::Level::DEBUG, "", s)
    //               //                 };
    //               //                 let _enter = span.enter();
    //               //                 let mut samples = comp.samples;
    //               //                 let (mut qs, mut dists) = (vec![], vec![]);
    //               //                 for dist in comp.dists.iter() {
    //               //                     let theta_q;
    //               //                     // FIXME: should be aggregating the stats somewhere
    //               //                     debug!("using optimizations: {}", self.opts.sample_pruning);
    //               //                     if self.opts.sample_pruning {
    //               //                         (theta_q, _) = crate::inference::calculate_wmc_prob(
    //               //                             self.mgr,
    //               //                             &wmc_params,
    //               //                             &var_order,
    //               //                             *dist, // TODO switch from *dist
    //               //                             accept,
    //               //                             BddPtr::PtrTrue, // TODO &samples
    //               //                         );
    //               //                     } else {
    //               //                         let sample_dist = self.mgr.and(samples, *dist);
    //               //                         (theta_q, _) = crate::inference::calculate_wmc_prob(
    //               //                             self.mgr,
    //               //                             &wmc_params,
    //               //                             &var_order,
    //               //                             sample_dist, // TODO switch from *dist
    //               //                             accept,
    //               //                             samples, // TODO &samples
    //               //                         );
    //               //                     }

    //               //                     let sample = match self.rng.as_mut() {
    //               //                         Some(rng) => {
    //               //                             let bern = Bernoulli::new(theta_q).unwrap();
    //               //                             bern.sample(rng)
    //               //                         }
    //               //                         None => sample_det,
    //               //                     };
    //               //                     qs.push(Probability::new(if sample {
    //               //                         theta_q
    //               //                     } else {
    //               //                         1.0 - theta_q
    //               //                     }));
    //               //                     self.pq.q *= if sample { theta_q } else { 1.0 - theta_q };
    //               //                     self.pq.p *= if sample { theta_q } else { 1.0 - theta_q };
    //               //                     let sampled_value = BddPtr::from_bool(sample);
    //               //                     dists.push(sampled_value);

    //               //                     if !self.opts.sample_pruning {
    //               //                         // sample in sequence. A smarter sample would compile
    //               //                         // all samples of a multi-rooted BDD, but I need to futz
    //               //                         // with rsdd's fold
    //               //                         let dist_holds = self.mgr.iff(*dist, sampled_value);
    //               //                         samples = self.mgr.and(samples, dist_holds);
    //               //                     }
    //               //                 }

    //               //                 debug!("final dists:   {}", renderbdds(&dists));
    //               //                 debug!("final samples: {}", samples.print_bdd());
    //               //                 debug!("using optimizations: {}", self.opts.sample_pruning);
    //               //                 let c = Output {
    //               //                     dists,
    //               //                     accept,
    //               //                     samples,
    //               //                     weightmap: comp.weightmap.clone(),
    //               //                     // any dangling references will be treated as
    //               //                     // constant and, thus, ignored -- information
    //               //                     // about this will live only in the propagated
    //               //                     // weight
    //               //                     substitutions: comp.substitutions.clone(),
    //               //                     probabilities: qs,
    //               //                     // importance: I::Weight(1.0),
    //               //                     ssubstitutions: ctx.ssubstitutions.clone(),
    //               //                     sout: None,
    //               //                 };
    //               //                 debug_step!("sample", ctx, c);
    //               //                 fin.push(c);
    //               //                 if self.rng.is_some() {
    //               //                     break;
    //               //                 }
    //               //             }
    //               //             Ok(fin)
    //               //         })
    //               //         .collect::<Result<Vec<Vec<Output>>>>()?
    //               //         .into_iter()
    //               //         .flatten()
    //               //         .collect();

    //               //     Ok((c.clone(), SExact(Box::new(c), Box::new(etr))))
    //               // }
    //         }
    //     }
}
