use crate::annotate::grammar::*;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::utils::render::*;
use crate::*;
use itertools::*;
use num_traits::*;
use rand::distributions::{Bernoulli, Distribution};
use rand::rngs::StdRng;
use rand::SeedableRng;
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
use rsdd::builder::cache::*;
use rsdd::repr::bdd::*;
use rsdd::repr::ddnnf::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::*;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::string::String;
use tracing::*;

pub use crate::data::context::Context;
pub use crate::data::errors::{CompileError, Result};
pub use crate::data::importance::{Importance, I};
pub use crate::data::output::{Compiled, Output, SubstMap};
pub use crate::data::{Weight, WeightMap};
use CompileError::*;

use super::anf::*;
use super::grammar::*;

pub const SQRT_2PI: f64 = 2.5066282746310005024157652848110452530069867406099;

macro_rules! ixspan {
    ($name:literal) => {
        tracing::span!(tracing::Level::DEBUG, $name)
    };
    ($name:literal, $ix:expr) => {
        if $ix == 0 {
            tracing::span!(tracing::Level::DEBUG, $name)
        } else {
            tracing::span!(tracing::Level::DEBUG, $name, i = $ix)
        }
    };
}

pub fn eval_eprj<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    i: usize,
    a: &'a AnfAnn<EVal>,
) -> Result<(
    Output,
    usize,
    AnfTr<EVal>,
    &'a dyn Fn(Compiled, usize, AnfTr<EVal>) -> EExprTr,
)> {
    let (mut o, atr, _) = eval_eanf(mgr, ctx, a)?;
    o.dists = vec![o.dists[i]];
    Ok((o, i, atr, &move |c, i, atr| {
        EExpr::EPrj(Box::new(c), i, Box::new(atr))
    }))
}

pub fn eval_eprod<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    anfs: &'a Vec<Anf<Annotated, EVal>>,
) -> Result<(
    Output,
    Vec<Anf<Trace, EVal>>,
    &'a dyn Fn(Compiled, Vec<Anf<Trace, EVal>>) -> EExprTr,
)> {
    let (dists, atrs) = anfs.iter().fold(Ok((vec![], vec![])), |res, a| {
        let (distsfin, mut atrs_fin) = res?;
        let (o, atr, _) = eval_eanf(mgr, ctx, a)?;
        let dists = distsfin.iter().chain(&o.dists).cloned().collect_vec();
        atrs_fin.push(atr);
        Ok((dists, atrs_fin))
    })?;
    let flen = dists.len();
    let o = Output {
        dists,
        accept: ctx.accept,
        samples: ctx.samples,
        weightmap: ctx.weightmap.clone(),
        substitutions: ctx.substitutions.clone(),
        probabilities: vec![Probability::new(1.0); flen],
        ssubstitutions: ctx.ssubstitutions.clone(),
        importance: I::Weight(1.0),
        sout: None,
    };
    Ok((o, atrs, &move |c, atrs| EExpr::EProd(Box::new(c), atrs)))
}

pub fn eval_eflip<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    d: &'a BddVar,
    param: f64,
) -> Result<(Output, f64, &'a dyn Fn(Compiled, f64) -> EExprTr)> {
    let mut weightmap = ctx.weightmap.clone();
    weightmap.insert(d.label, param);
    let o = Output {
        dists: vec![mgr.var(d.label, true)],
        accept: ctx.accept,
        samples: ctx.samples,
        weightmap,
        substitutions: ctx.substitutions.clone(),
        probabilities: vec![Probability::new(1.0)],
        importance: I::Weight(1.0),
        ssubstitutions: ctx.ssubstitutions.clone(),
        sout: None,
    };
    Ok((o, param, &move |c, f| EExpr::EFlip(Box::new(c), f)))
}

pub fn eval_eobserve<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    a: &'a Anf<Annotated, EVal>,
    opts: &'a Opts,
) -> Result<(
    Output,
    AnfTr<EVal>,
    &'a dyn Fn(Compiled, AnfTr<EVal>) -> EExprTr,
)> {
    let (comp, atr, _) = eval_eanf(mgr, ctx, a)?;

    debug!("In. Accept {}", &ctx.accept.print_bdd());
    debug!("Comp. dist {}", renderbdds(&comp.dists));
    debug!("weightmap  {:?}", ctx.weightmap);
    let dist = comp
        .dists
        .into_iter()
        .fold(ctx.accept, |global, cur| mgr.and(global, cur));
    let var_order = opts.order.clone();
    let wmc_params = ctx.weightmap.as_params(opts.max_label);
    let avars = crate::utils::variables(dist);
    for (i, var) in avars.iter().enumerate() {
        debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
    }
    debug!("WMCParams  {:?}", wmc_params);
    debug!("VarOrder   {:?}", var_order);
    debug!("Accept     {}", dist.print_bdd());
    // FIXME: should be aggregating these stats somewhere
    debug!("using optimizations: {}", opts.sample_pruning);

    let (wmc, _) = crate::inference::calculate_wmc_prob(
        mgr,
        &wmc_params,
        &var_order,
        dist,
        ctx.accept,
        if opts.sample_pruning {
            BddPtr::PtrTrue
        } else {
            ctx.samples
        },
    );

    let importance = I::Weight(wmc);
    debug!("IWeight    {}", importance.weight());

    let o = Output {
        dists: vec![BddPtr::PtrTrue],
        accept: dist,
        samples: ctx.samples,
        weightmap: ctx.weightmap.clone(),
        substitutions: ctx.substitutions.clone(),
        probabilities: vec![Probability::new(1.0)],
        importance,
        ssubstitutions: ctx.ssubstitutions.clone(),
        sout: None,
    };
    Ok((o, atr, &move |c, atr| {
        EExpr::EObserve(Box::new(c), Box::new(atr))
    }))
}

pub fn eval_eite_predicate<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    cond: &'a AnfAnn<EVal>,
    opts: &'a Opts,
) -> Result<(BddPtr, AnfTr<EVal>, (Probability, Probability))> {
    let (pred, atr, _) = eval_eanf(mgr, ctx, cond)?;
    if !pred.dists.len() == 1 {
        return Err(TypeError(format!(
            "Expected EBool for ITE condition\nGot: {cond:?}\n{ctx:?}",
        )));
    }

    let pred_dist = pred.dists[0];
    let var_order = opts.order.clone();
    let wmc_params = ctx.weightmap.as_params(opts.max_label);

    // FIXME : should be adding stats somewhere
    let mut wmc_opt_h = |pred_dist| {
        Probability::new(
            crate::inference::calculate_wmc_prob(
                mgr,
                &wmc_params,
                &var_order,
                pred_dist,
                ctx.accept,
                // TODO if switching to samples_opt, no need to use ctx.
                if opts.sample_pruning {
                    BddPtr::PtrTrue
                } else {
                    ctx.samples
                },
            )
            .0,
        )
    };
    let wmc_true = wmc_opt_h(pred_dist);
    let wmc_false = wmc_opt_h(pred_dist.neg());
    Ok((pred_dist, atr, (wmc_true, wmc_false)))
}
pub fn eval_eite_output<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    pred: (BddPtr, AnfTr<EVal>),
    truthy_branch: (Probability, Output),
    falsey_branch: (Probability, Output),
    opts: &'a Opts,
) -> Result<Output> {
    let (pred_dist, pred_anf) = pred;
    let (wmc_true, truthy) = truthy_branch;
    let (wmc_false, falsey) = falsey_branch;
    let dists = izip!(&truthy.dists, &falsey.dists)
        .map(|(tdist, fdist)| {
            let dist_l = mgr.and(pred_dist, *tdist);
            let dist_r = mgr.and(pred_dist.neg(), *fdist);
            mgr.or(dist_l, dist_r)
        })
        .collect_vec();

    let samples = if opts.sample_pruning {
        truthy.samples
    } else {
        mgr.and(truthy.samples, falsey.samples)
    };

    let accept_l = mgr.and(pred_dist, truthy.accept);
    let accept_r = mgr.and(pred_dist.neg(), falsey.accept);
    let accept = mgr.or(accept_l, accept_r);
    let accept = mgr.and(accept, ctx.accept);

    let mut substitutions = truthy.substitutions.clone();
    substitutions.extend(falsey.substitutions.clone());
    let mut weightmap = truthy.weightmap.clone();
    weightmap.weights.extend(falsey.weightmap.clone());

    let probabilities = izip!(&truthy.probabilities, &falsey.probabilities)
        .map(|(t, f)| (*t * wmc_true + *f * wmc_false))
        .collect_vec();

    debug!("=============================");
    let importance_true = truthy.importance.pr_mul(wmc_true);
    let importance_false = falsey.importance.pr_mul(wmc_false);
    debug!(
        "importance_true {:?}, importance_false {:?}",
        importance_true, importance_false
    );
    let importance = importance_true + importance_false;
    debug!("importance {:?}", importance);
    debug!("=============================");

    let o = Output {
        dists,
        accept,
        samples,
        weightmap,
        substitutions,
        probabilities,
        importance,
        ssubstitutions: ctx.ssubstitutions.clone(),
        sout: None,
    };
    Ok(o)
}
pub fn eval_elet_output<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    bound: Output,
    body: Output,
    opts: &'a Opts,
) -> Result<Output> {
    let accept = mgr.and(body.accept, ctx.accept);
    let samples = if opts.sample_pruning {
        ctx.samples
    } else {
        mgr.and(body.samples, ctx.samples)
    };
    let probabilities = izip!(bound.probabilities.clone(), body.probabilities)
        .map(|(p1, p2)| p1 * p2)
        .collect_vec();
    let importance = I::Weight(bound.importance.weight() * body.importance.weight());

    let c = Output {
        dists: body.dists,
        accept,
        samples,
        substitutions: body.substitutions.clone(),
        weightmap: body.weightmap,
        probabilities,
        importance,
        ssubstitutions: ctx.ssubstitutions.clone(),
        sout: None,
    };
    Ok(c)
}

pub struct Opts {
    pub sample_pruning: bool,
    pub max_label: u64,
    pub order: VarOrder,
}

pub struct State<'a> {
    pub opts: Opts,
    pub mgr: &'a mut Mgr,
    pub rng: Option<&'a mut StdRng>, // None implies "debug mode"
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
        }
    }
    pub fn p(&self) -> f64 {
        self.pq.p
    }
    pub fn q(&self) -> f64 {
        self.pq.q
    }

    pub fn eval_program(
        &mut self,
        prog: &Program<Annotated>,
    ) -> Result<(Compiled, Program<Trace>)> {
        match prog {
            Program::SBody(e) => {
                let (c, e) = self.eval_sexpr(Context::default(), e)?;
                Ok((c, Program::SBody(e)))
            }
            Program::EBody(e) => {
                let (c, e) = self.eval_eexpr(Context::default(), e)?;
                Ok((c, Program::EBody(e)))
            }
        }
    }

    pub fn eval_eexpr(&mut self, ctx: Context, e: &EExprAnn) -> Result<(Compiled, EExprTr)> {
        use EExpr::*;
        match e {
            EAnf(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "anf");
                let _enter = span.enter();
                let (o, a, mk) = eval_eanf(self.mgr, &ctx, a)?;
                debug_step!("anf", ctx, &o);
                let c = Compiled::Output(o);
                Ok((c.clone(), mk(c, a)))
            }
            EPrj(_, i, a) => {
                if i > &1 {
                    let span = tracing::span!(tracing::Level::DEBUG, "prj", i);
                    let _enter = span.enter();
                    debug!("{:?}", a);
                }
                let (o, i, a, mk) = eval_eprj(self.mgr, &ctx, *i, a)?;
                if i > 1 {
                    debug_step!(&format!("prj@{}", i), ctx, o);
                }
                let c = Compiled::Output(o);
                Ok((c.clone(), mk(c, i, a)))
            }
            EProd(_, anfs) => {
                let span = tracing::span!(tracing::Level::DEBUG, "prod");
                let _enter = span.enter();
                debug!("{:?}", anfs);

                let (o, atrs, mk) = eval_eprod(self.mgr, &ctx, anfs)?;

                debug_step!("prod", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), mk(c, atrs)))
            }
            EFlip(d, param) => {
                // FIXME: is this... necessary? I think it's just for debugging
                //o let flip = (param * 100.0).round() / 100.0;
                let flip = *param;
                let span = tracing::span!(tracing::Level::DEBUG, "", flip);
                let _enter = span.enter();

                let (o, f, mk) = eval_eflip(self.mgr, &ctx, d, flip)?;

                debug_step!("flip", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), mk(c, f)))
            }
            EObserve(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "observe");
                let _enter = span.enter();
                let (o, atr, mk) = eval_eobserve(self.mgr, &ctx, a, &self.opts)?;

                let Importance::Weight(theta) = o.importance;
                self.pq.p *= theta; // <<<<<<<<<<<<<<<<<<<<

                debug_step!("observe", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), mk(c, atr)))
            }
            EIte(_, cond, t, f) => {
                let span = tracing::span!(tracing::Level::DEBUG, "ite");
                let _enter = span.enter();
                let (pred_dist, pred_anf, (wmc_true, wmc_false)) =
                    eval_eite_predicate(self.mgr, &ctx, cond, &self.opts)?;

                debug!("=============================");
                debug!("wmc_true {}, wmc_false {}", wmc_true, wmc_false);
                debug!("=============================");

                let span = ixspan!("truthy");
                let _enter = span.enter();
                let (ct, ttr) = self.eval_eexpr(ctx.clone(), t)?;
                drop(_enter);

                let mut os: Vec<Output> = vec![];
                let mut mftr: Option<EExprTr> = None;
                for (ix, truthy) in ct.into_iter().enumerate() {
                    let span = ixspan!("truthy", ix);
                    let _enter = span.enter();

                    let (cf, ftr_) = self.eval_eexpr(ctx.clone(), f)?;
                    mftr = Some(ftr_);

                    let mut fcs: Vec<Output> = vec![];
                    for (ix, falsey) in cf.into_iter().enumerate() {
                        let span = ixspan!("falsey", ix);
                        let _enter = span.enter();
                        if truthy.dists.len() != falsey.dists.len() {
                            return Err(TypeError(format!("Expected both branches of ITE to return same len tuple\nGot (left): {:?}\nGot (right):{:?}", truthy.dists.len(), falsey.dists.len(),)));
                        }
                        let c: Output = eval_eite_output(
                            self.mgr,
                            &ctx,
                            (pred_dist, pred_anf.clone()),
                            (wmc_true, truthy.clone()),
                            (wmc_false, falsey),
                            &self.opts,
                        )?;
                        debug_step!("ite", ctx, c);
                        fcs.push(c);
                    }
                    os.extend(fcs);
                }
                let ftr = mftr.unwrap();
                let outs: Compiled = os.into_iter().collect();
                Ok((
                    outs.clone(),
                    EIte(
                        Box::new(outs),
                        Box::new(pred_anf),
                        Box::new(ttr),
                        Box::new(ftr),
                    ),
                ))
            }
            ELetIn(d, s, ebound, ebody) => {
                let let_in_span = tracing::span!(Level::DEBUG, "let", var = s);
                let _enter = let_in_span.enter();

                let (cbound, eboundtr) = self.eval_eexpr(ctx.clone(), ebound)?;

                let mut outs: Vec<Output> = vec![];
                let mut mbody: Option<EExprTr> = None;

                for (ix, bound) in cbound.into_iter().enumerate() {
                    let ix_span = tracing::span!(Level::DEBUG, "", ix);
                    let _enter = ix_span.enter();

                    let mut newctx = Context::from_compiled(&bound);
                    newctx
                        .substitutions
                        .insert(d.id(), (bound.dists.clone(), Var::Named(d.clone())));
                    let (bodies, bodiestr) = self.eval_eexpr(newctx, ebody)?;

                    let cbodies = bodies
                        .into_iter()
                        .enumerate()
                        .map(|(ix, body)| {
                            let span = ixspan!("", ix);
                            let _enter = span.enter();

                            let c =
                                eval_elet_output(self.mgr, &ctx, bound.clone(), body, &self.opts)?;

                            debug_step!(format!("let-in {}", s), ctx, c);
                            Ok(c)
                        })
                        .collect::<Result<Vec<Output>>>()?;

                    outs.extend(cbodies);
                    mbody = Some(bodiestr);
                }
                let ebodytr = mbody.unwrap();
                let outs: Compiled = outs.into_iter().collect();
                Ok((
                    outs.clone(),
                    ELetIn(
                        Box::new(outs),
                        s.clone(),
                        Box::new(eboundtr),
                        Box::new(ebodytr),
                    ),
                ))
            }
            ESample((), sexpr) => {
                let (c, s) = self.eval_sexpr(ctx, sexpr)?;
                Ok((c.clone(), ESample(Box::new(c), Box::new(s))))
            }
        }
    }

    pub fn eval_sexpr(&mut self, ctx: Context, e: &SExprAnn) -> Result<(Compiled, SExprTr)> {
        use SExpr::*;
        match e {
            SAnf(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sanf");
                let _enter = span.enter();
                debug!("anf: {:?}", a);

                let (o, a, mk) = eval_sanf(&ctx, a)?;
                let c = Compiled::Output(o);
                Ok((c.clone(), mk(c, a)))
            }
            SBern(_, param) => {
                let span = tracing::span!(tracing::Level::DEBUG, "bernoulli", p = param);
                let _enter = span.enter();

                let dist = statrs::distribution::Bernoulli::new(*param).unwrap();
                let x = match self.rng.as_mut() {
                    Some(rng) => dist.sample(rng),
                    None => dist.sample(&mut rand::thread_rng()),
                };
                let x = x != 0.0;
                let weight = statrs::distribution::Discrete::pmf(&dist, x as u64);
                let mut o = Output::for_sample_lang(&ctx);
                self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                debug!(q = self.pq.q, test = "", p = self.pq.p);

                o.sout = Some(SVal::SBool(x));
                let c = Compiled::from_output(o);
                Ok((c.clone(), SBern(Box::new(c), *param)))
            }
            SDiscrete(_, ps) => {
                let span = tracing::span!(tracing::Level::DEBUG, "discrete");
                let _enter = span.enter();

                let dist = statrs::distribution::Categorical::new(ps).unwrap();
                let x = match self.rng.as_mut() {
                    Some(rng) => dist.sample(rng),
                    None => dist.sample(&mut rand::thread_rng()),
                };
                let x = x as u64;
                let weight = statrs::distribution::Discrete::pmf(&dist, x); // internally, this sample was cast as f64, so this is okay
                let mut o = Output::for_sample_lang(&ctx);
                self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                o.sout = Some(SVal::SInt(x));
                let c = Compiled::from_output(o);
                Ok((c.clone(), SDiscrete(Box::new(c), ps.clone())))
            }
            SDirichlet(_, ps) => {
                let span = tracing::span!(tracing::Level::DEBUG, "dirichlet");
                let _enter = span.enter();

                let dist = statrs::distribution::Dirichlet::new(ps.to_vec()).unwrap();
                let x = match self.rng.as_mut() {
                    Some(rng) => dist.sample(rng),
                    None => dist.sample(&mut rand::thread_rng()),
                };
                let weight = statrs::distribution::Continuous::pdf(&dist, &x);
                let mut o = Output::for_sample_lang(&ctx);
                self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                let x = x.data.as_vec().to_vec();
                o.sout = Some(SVal::SFloatVec(x));
                let c = Compiled::from_output(o);
                Ok((c.clone(), SDiscrete(Box::new(c), ps.clone())))
            }
            SUniform(_, lo, hi) => {
                let span = tracing::span!(tracing::Level::DEBUG, "uniform", l = lo, h = hi);
                let _enter = span.enter();

                let dist = statrs::distribution::Uniform::new(*lo, *hi).unwrap();
                let x = match self.rng.as_mut() {
                    Some(rng) => dist.sample(rng),
                    None => dist.sample(&mut rand::thread_rng()),
                };
                let weight = statrs::distribution::Continuous::pdf(&dist, x);
                let mut o = Output::for_sample_lang(&ctx);
                self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                o.sout = Some(SVal::SFloat(x));
                let c = Compiled::from_output(o);
                Ok((c.clone(), SUniform(Box::new(c), *lo, *hi)))
            }
            SNormal(_, mn, sd) => {
                let span = tracing::span!(tracing::Level::DEBUG, "normal", m = mn, sd = sd);
                let _enter = span.enter();

                let dist = statrs::distribution::Normal::new(*mn, *sd).unwrap();
                let x = match self.rng.as_mut() {
                    Some(rng) => dist.sample(rng),
                    None => dist.sample(&mut rand::thread_rng()),
                };

                let weight = statrs::distribution::Continuous::pdf(&dist, x);
                let mut o = Output::for_sample_lang(&ctx);
                self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                o.sout = Some(SVal::SFloat(x));
                let c = Compiled::from_output(o);
                Ok((c.clone(), SNormal(Box::new(c), *mn, *sd)))
            }
            SBeta(_, a, b) => {
                let span = tracing::span!(tracing::Level::DEBUG, "beta", a = a, b = b);
                let _enter = span.enter();
                let dist = statrs::distribution::Beta::new(*a, *b).unwrap();
                let x = match self.rng.as_mut() {
                    Some(rng) => dist.sample(rng),
                    None => dist.sample(&mut rand::thread_rng()),
                };
                let weight = statrs::distribution::Continuous::pdf(&dist, x);
                let mut o = Output::for_sample_lang(&ctx);
                self.pq.p *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                self.pq.q *= weight; // <<<<<<<<<<<<<<<<<<<<<<<<<<<
                o.sout = Some(SVal::SFloat(x));
                let c = Compiled::from_output(o);
                Ok((c.clone(), SBeta(Box::new(c), *a, *b)))
            }
            SLetIn(d, name, bindee, body) => {
                let span = tracing::span!(tracing::Level::DEBUG, "slet", v = name);
                let _enter = span.enter();
                let (cbindee, ebindee) = self.eval_sexpr(ctx, bindee)?;
                // debug!("{:?}", cbindee);
                debug!("BINDEE:: {:?}", ebindee);
                let vbindee = cbindee
                    .as_output()
                    .unwrap()
                    .sout
                    .expect(&format!("bindee {name} doesn't return a value"));
                debug!("VALUE:: {:?}", vbindee);

                let mut ctx = Context::from_compiled(&cbindee.as_output().unwrap());
                ctx.ssubstitutions.insert(d.id(), vbindee);
                let (cbody, ebody) = self.eval_sexpr(ctx, body)?;
                debug!("SOMETHING");
                Ok((
                    cbody.clone(),
                    SSeq(Box::new(cbody), Box::new(ebindee), Box::new(ebody)),
                ))
            }
            SSeq(_, e0, e1) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample-seq");
                let _enter = span.enter();
                let (c0, e0) = self.eval_sexpr(ctx, e0)?;
                let ctx = Context::from_compiled(&c0.as_output().unwrap());
                let (c1, e1) = self.eval_sexpr(ctx, e1)?;
                Ok((c1.clone(), SSeq(Box::new(c1), Box::new(e0), Box::new(e1))))
            }
            SIte(_, guard, truthy, falsey) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample-ite");
                let _enter = span.enter();
                let (cguard, eguard, _) = crate::compile::anf::eval_sanf(&ctx, guard)?;
                let (ctruthy, etruthy) = self.eval_sexpr(ctx.clone(), truthy)?;
                let (cfalsey, efalsey) = self.eval_sexpr(ctx, falsey)?;
                todo!()
            }
            SExact(_, eexpr) => {
                let span = tracing::span!(tracing::Level::DEBUG, "exact");
                let _enter = span.enter();

                let (comp, etr) = self.eval_eexpr(ctx.clone(), eexpr)?;
                let c: Compiled = comp
                    .into_iter()
                    .enumerate()
                    .map(|(ix, comp)| {
                        let span = ixspan!("", ix);
                        let _enter = span.enter();

                        let wmc_params = comp.weightmap.as_params(self.opts.max_label);
                        let var_order = self.opts.order.clone();
                        debug!("Incm accept {}", &ctx.accept.print_bdd());
                        debug!("Comp accept {}", comp.accept.print_bdd());
                        debug!("Comp distrb {}", renderbdds(&comp.dists));

                        debug!("weight_map {:?}", &comp.weightmap);
                        debug!("WMCParams  {:?}", wmc_params);
                        debug!("VarOrder   {:?}", var_order);
                        let accept = comp.accept;

                        let mut fin = vec![];

                        for sample_det in [true, false] {
                            let span = if sample_det {
                                tracing::span!(tracing::Level::DEBUG, "")
                            } else {
                                let s = false;
                                tracing::span!(tracing::Level::DEBUG, "", s)
                            };
                            let _enter = span.enter();
                            let mut samples = comp.samples;
                            let (mut qs, mut dists) = (vec![], vec![]);
                            for dist in comp.dists.iter() {
                                let theta_q;
                                // FIXME: should be aggregating the stats somewhere
                                debug!("using optimizations: {}", self.opts.sample_pruning);
                                if self.opts.sample_pruning {
                                    (theta_q, _) = crate::inference::calculate_wmc_prob(
                                        self.mgr,
                                        &wmc_params,
                                        &var_order,
                                        *dist, // TODO switch from *dist
                                        accept,
                                        BddPtr::PtrTrue, // TODO &samples
                                    );
                                } else {
                                    let sample_dist = self.mgr.and(samples, *dist);
                                    (theta_q, _) = crate::inference::calculate_wmc_prob(
                                        self.mgr,
                                        &wmc_params,
                                        &var_order,
                                        sample_dist, // TODO switch from *dist
                                        accept,
                                        samples, // TODO &samples
                                    );
                                }

                                let sample = match self.rng.as_mut() {
                                    Some(rng) => {
                                        let bern = Bernoulli::new(theta_q).unwrap();
                                        bern.sample(rng)
                                    }
                                    None => sample_det,
                                };
                                qs.push(Probability::new(if sample {
                                    theta_q
                                } else {
                                    1.0 - theta_q
                                }));
                                self.pq.q *= if sample { theta_q } else { 1.0 - theta_q };
                                self.pq.p *= if sample { theta_q } else { 1.0 - theta_q };
                                let sampled_value = BddPtr::from_bool(sample);
                                dists.push(sampled_value);

                                if !self.opts.sample_pruning {
                                    // sample in sequence. A smarter sample would compile
                                    // all samples of a multi-rooted BDD, but I need to futz
                                    // with rsdd's fold
                                    let dist_holds = self.mgr.iff(*dist, sampled_value);
                                    samples = self.mgr.and(samples, dist_holds);
                                }
                            }

                            debug!("final dists:   {}", renderbdds(&dists));
                            debug!("final samples: {}", samples.print_bdd());
                            debug!("using optimizations: {}", self.opts.sample_pruning);
                            let c = Output {
                                dists,
                                accept,
                                samples,
                                weightmap: comp.weightmap.clone(),
                                // any dangling references will be treated as
                                // constant and, thus, ignored -- information
                                // about this will live only in the propagated
                                // weight
                                substitutions: comp.substitutions.clone(),
                                probabilities: qs,
                                importance: I::Weight(1.0),
                                ssubstitutions: ctx.ssubstitutions.clone(),
                                sout: None,
                            };
                            debug_step!("sample", ctx, c);
                            fin.push(c);
                            if self.rng.is_some() {
                                break;
                            }
                        }
                        Ok(fin)
                    })
                    .collect::<Result<Vec<Vec<Output>>>>()?
                    .into_iter()
                    .flatten()
                    .collect();

                Ok((c.clone(), SExact(Box::new(c), Box::new(etr))))
            }
        }
    }
}
