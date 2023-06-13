use crate::annotate::grammar::*;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::utils::render::*;
use crate::*;
use either::{Either, Either::*};
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

pub fn eval_eanf<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    a: &'a AnfAnn<EVal>,
) -> Result<(
    Output,
    Anf<Trace, EVal>,
    &'a dyn Fn(Compiled, AnfTr<EVal>) -> EExprTr,
)> {
    let (o, anf) = eval_anf(mgr, ctx, a, &mut |_, v| match v {
        EVal::EBool(b) => {
            let c = Output::from_anf_dists(ctx, vec![BddPtr::from_bool(*b)]);
            Ok((c.clone(), Anf::AVal(Box::new(c), EVal::EBool(*b))))
        }
        EVal::EProd(b) => Err(CompileError::Todo()),
    })?;
    Ok((o, anf, &move |c, a| EExpr::EAnf(Box::new(c), Box::new(a))))
}

pub fn eval_sanf<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    a: &'a AnfAnn<SVal>,
) -> Result<(
    Output,
    AnfTr<SVal>,
    &'a dyn Fn(Compiled, AnfTr<SVal>) -> SExprTr,
)> {
    let (o, anf) = eval_anf(mgr, ctx, a, &mut |_, v| match v {
        SVal::SBool(b) => {
            let c = Output::from_anf_dists(ctx, vec![BddPtr::from_bool(*b)]);
            Ok((c.clone(), Anf::AVal(Box::new(c), SVal::SBool(*b))))
        }
    })?;
    Ok((o, anf, &move |piled, anf| {
        SExpr::SAnf(Box::new(piled), Box::new(anf))
    }))
}

pub fn eval_eprj<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    a: &'a AnfAnn<EVal>,
    i: usize,
) -> Result<(
    Output,
    AnfTr<EVal>,
    &'a dyn Fn(Compiled, AnfTr<EVal>) -> EExprTr,
)> {
    let (o, atr) = eval_anf(mgr, ctx, a, &mut |mgr, v| {
        let (mut o, atr, _) = eval_eanf(mgr, ctx, a)?;
        let dists = o.dists;
        o.dists = vec![dists[i]];
        Ok((o, atr))
    })?;
    Ok((o, atr, &move |c, atr| {
        EExpr::EAnf(Box::new(c), Box::new(atr))
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
        importance: I::Weight(1.0),
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
    };
    Ok((o, param, &move |c, f| EExpr::EFlip(Box::new(c), f)))
}

pub fn eval_sflip<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    param: f64,
) -> Result<(Output, f64, &'a dyn Fn(Compiled, f64) -> SExprTr)> {
    let o = Output::from_anf_dists(ctx, vec![]);
    // let o = Output {
    //     dists: vec![],
    //     accept: ctx.accept,
    //     samples: ctx.samples,
    //     weightmap,
    //     substitutions: ctx.substitutions.clone(),
    //     probabilities: vec![Probability::new(1.0)],
    //     importance: I::Weight(1.0),
    // };
    Ok((o, param, &move |c, f| SExpr::SFlip(Box::new(c), f)))
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
    };
    Ok(c)
}

pub struct Opts {
    pub sample_pruning: bool,
    pub max_label: u64,
    pub order: VarOrder,
}

pub struct State<'a> {
    opts: Opts,
    mgr: &'a mut Mgr,
    rng: Option<&'a mut StdRng>, // None implies "debug mode"
}

impl<'a> State<'a> {
    pub fn eval_program(
        &mut self,
        prog: &'a Program<Annotated>,
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

    pub fn eval_eexpr(&mut self, ctx: Context, e: &'a EExprAnn) -> Result<(Compiled, EExprTr)> {
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
                let (o, a, mk) = eval_eprj(self.mgr, &ctx, a, *i)?;
                if i > &1 {
                    debug_step!(&format!("prj@{}", i), ctx, o);
                }
                let c = Compiled::Output(o);
                Ok((c.clone(), mk(c, a)))
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
                let flip = (param * 100.0).round() / 100.0;
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

                // let lbl = d.var.label;
                // if we produce multiple worlds, we must account for them all
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
                    // Ok((cbodies, bodiestr))
                    // })
                    // .collect::<Result<Vec<(Vec<Output>, EExprTr)>>>()?
                    // .into_iter()
                    // .fold(
                    //     (vec![], None),
                    //     |(mut outs, mbody), (compiled_outs, body)| {
                    //         outs.extend(compiled_outs);
                    //         (outs, Some(body))
                    //     },
                    // );
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
                panic!("no longer acceptable!");
            }
            ESample2((), sexpr) => {
                let (c, s) = self.eval_sexpr(ctx, sexpr)?;
                Ok((c.clone(), ESample2(Box::new(c), Box::new(s))))
            }
        }
    }

    pub fn eval_sexpr(&mut self, ctx: Context, e: &'a SExprAnn) -> Result<(Compiled, SExprTr)> {
        use SExpr::*;
        match e {
            SAnf(_, a) => {
                let (o, a, mk) = eval_sanf(self.mgr, &ctx, a)?;
                let c = Compiled::Output(o);
                Ok((c.clone(), mk(c, a)))
            }
            SFlip(_, param) => {
                let param = *param;
                let sample = match self.rng.as_mut() {
                    Some(rng) => {
                        let bern = Bernoulli::new(param).unwrap();
                        bern.sample(rng)
                    }
                    None => todo!(),
                };
                let weight = if sample { param } else { 1.0 - param };
                todo!()
            }
            SLetIn(_, name, bindee, body) => todo!(),
            SSeq(_, e0, e1) => todo!(),
            SIte(_, guard, struthy, sfalsey) => todo!(),
            SExact(_, eexpr) => {
                let (c, e) = self.eval_eexpr(ctx, eexpr)?;
                Ok((c.clone(), SExact(Box::new(c), Box::new(e))))
            }
        }
    }
}

// //             ESample(_, e) => {
// //                 let span = tracing::span!(tracing::Level::DEBUG, "sample");
// //                 let _enter = span.enter();

// //                 let (comp, etr) = self.eval_eexpr(ctx, e)?;
// //                 let c: Compiled = comp
// //                     .into_iter()
// //                     .enumerate()
// //                     .map(|(ix, comp)| {
// //                         let span = if ix == 0 {
// //                             tracing::span!(tracing::Level::DEBUG, "")
// //                         } else {
// //                             tracing::span!(tracing::Level::DEBUG, "", ix)
// //                         };
// //                         let _enter = span.enter();

// //                         let wmc_params = comp.weightmap.as_params(self.max_label);
// //                         let var_order = self.order.clone();
// //                         debug!("Incm accept {}", &ctx.accept.print_bdd());
// //                         debug!("Comp accept {}", comp.accept.print_bdd());
// //                         debug!("Comp distrb {}", renderbdds(&comp.dists));

// //                         debug!("weight_map {:?}", &comp.weightmap);
// //                         debug!("WMCParams  {:?}", wmc_params);
// //                         debug!("VarOrder   {:?}", var_order);
// //                         let accept = comp.accept;

// //                         let mut fin = vec![];

// //                         for sample_det in [true, false] {
// //                             let span = if sample_det {
// //                                 tracing::span!(tracing::Level::DEBUG, "")
// //                             } else {
// //                                 let s = false;
// //                                 tracing::span!(tracing::Level::DEBUG, "", s)
// //                             };
// //                             let _enter = span.enter();
// //                             let mut samples = comp.samples;
// //                             let (mut qs, mut dists) = (vec![], vec![]);
// //                             for dist in comp.dists.iter() {
// //                                 let theta_q;
// //                                 // FIXME: should be aggregating the stats somewhere
// //                                 debug!("using optimizations: {}", self.sample_pruning);
// //                                 if self.sample_pruning {
// //                                     (theta_q, _) = crate::inference::calculate_wmc_prob(
// //                                         self.mgr,
// //                                         &wmc_params,
// //                                         &var_order,
// //                                         *dist, // TODO switch from *dist
// //                                         accept,
// //                                         BddPtr::PtrTrue, // TODO &samples
// //                                     );
// //                                 } else {
// //                                     let sample_dist = self.mgr.and(samples, *dist);
// //                                     (theta_q, _) = crate::inference::calculate_wmc_prob(
// //                                         self.mgr,
// //                                         &wmc_params,
// //                                         &var_order,
// //                                         sample_dist, // TODO switch from *dist
// //                                         accept,
// //                                         samples, // TODO &samples
// //                                     );
// //                                 }

// //                                 let sample = match self.rng.as_mut() {
// //                                     Some(rng) => {
// //                                         let bern = Bernoulli::new(theta_q).unwrap();
// //                                         bern.sample(rng)
// //                                     }
// //                                     None => sample_det,
// //                                 };
// //                                 qs.push(Probability::new(if sample {
// //                                     theta_q
// //                                 } else {
// //                                     1.0 - theta_q
// //                                 }));
// //                                 let sampled_value = BddPtr::from_bool(sample);
// //                                 dists.push(sampled_value);

// //                                 if !self.sample_pruning {
// //                                     // sample in sequence. A smarter sample would compile
// //                                     // all samples of a multi-rooted BDD, but I need to futz
// //                                     // with rsdd's fold
// //                                     let dist_holds = self.mgr.iff(*dist, sampled_value);
// //                                     samples = self.mgr.and(samples, dist_holds);
// //                                 }
// //                             }
// //                             debug!("final dists:   {}", renderbdds(&dists));
// //                             debug!("final samples: {}", samples.print_bdd());
// //                             // println!("sample_pruning: {}", self.sample_pruning);
// //                             debug!("using optimizations: {}", self.sample_pruning);
// //                             let c = Output {
// //                                 dists,
// //                                 accept,
// //                                 samples,
// //                                 weightmap: comp.weightmap.clone(),
// //                                 // any dangling references will be treated as
// //                                 // constant and, thus, ignored -- information
// //                                 // about this will live only in the propagated
// //                                 // weight
// //                                 substitutions: comp.substitutions.clone(),
// //                                 probabilities: qs,
// //                                 importance: I::Weight(1.0),
// //                             };
// //                             debug_step!("sample", ctx, c);
// //                             fin.push(c);
// //                             if self.rng.is_some() {
// //                                 break;
// //                             }
// //                         }
// //                         Ok(fin)
// //                     })
// //                     .collect::<Result<Vec<Vec<Output>>>>()?
// //                     .into_iter()
// //                     .flatten()
// //                     .collect();

// //                 Ok((c.clone(), ESample(Box::new(c), Box::new(etr))))
// //             }
// //             ESample2(_, e) => {

// //             },
// //         }
// //     }
// // }
