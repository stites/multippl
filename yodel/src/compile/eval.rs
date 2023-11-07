use crate::annotate::grammar::*;
use crate::annotate::Fun;
use crate::grammar::*;
use crate::uniquify::grammar::{FnCall, FnId, UniqueId};
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
use super::sample::*;

pub fn eval_eite_predicate(
    state: &mut super::eval::State,
    ctx: &Ctx,
    cond: &AnfAnn<EVal>,
    opts: &Opts,
) -> Result<(BddPtr, (Probability, Probability))> {
    let pred = eval_eanf(state, &ctx, cond)?;
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
        let ss = ctx.exact.samples(state.mgr, opts.sample_pruning);
        Probability::new(
            crate::inference::calculate_wmc_prob(
                state.mgr,
                &wmc_params,
                &var_order,
                pred_dist,
                ctx.exact.accept.clone(),
                // TODO if switching to samples_opt, no need to use ctx.
                ss,
            )
            .0,
        )
    };
    let wmc_true = wmc_opt_h(pred_dist);
    let wmc_false = wmc_opt_h(pred_dist.neg());
    Ok((pred_dist, (wmc_true, wmc_false)))
}
#[cfg(feature = "debug_samples")]
fn mk_ite_output_samples(
    opts: &Opts,
    state: &mut super::eval::State,
    truthy_samples: &HashMap<BddPtr, bool>,
    falsy_samples: &HashMap<BddPtr, bool>,
) -> HashMap<BddPtr, bool> {
    let mut samples = HashMap::new();
    if !opts.sample_pruning {
        samples.extend(truthy_samples);
        samples.extend(falsy_samples);
    };
    samples
}
#[cfg(not(feature = "debug_samples"))]
fn mk_ite_output_samples(
    opts: &Opts,
    state: &mut super::eval::State,
    truthy_samples: &BddPtr,
    falsy_samples: &BddPtr,
) -> BddPtr {
    if !opts.sample_pruning {
        return state.mgr.and(*truthy_samples, *falsy_samples);
    };
    BddPtr::PtrTrue
}
pub fn eval_eite_output(
    state: &mut super::eval::State,
    ctx: &Ctx,
    pred: BddPtr,
    truthy_branch: (Probability, Output),
    falsey_branch: (Probability, Output),
    opts: &Opts,
) -> Result<EOutput> {
    let pred_dist = pred;
    let (wmc_true, truthy) = truthy_branch;
    let (wmc_false, falsey) = falsey_branch;
    let dist = match (&truthy.exact.out, &falsey.exact.out) {
        (Some(EVal::EBdd(tdist)), Some(EVal::EBdd(fdist))) => {
            let dist_l = state.mgr.and(pred_dist, *tdist);
            let dist_r = state.mgr.and(pred_dist.neg(), *fdist);
            let fin = state.mgr.or(dist_l, dist_r);
            EVal::EBdd(fin)
        }
        (t, f) => panic!(
            "typecheck failed to catch EIte with\ntruthy: {t:?}\nfalsey: {f:?}",
            t = t,
            f = f
        ),
    };
    // let dists = izip!(&truthy.exact.dists(), &falsey.exact.dists())
    //     .map(|(tdist, fdist)| {
    //         let dist_l = state.mgr.and(pred_dist, *tdist);
    //         let dist_r = state.mgr.and(pred_dist.neg(), *fdist);
    //         let fin = state.mgr.or(dist_l, dist_r);
    //         EVal::EBdd(fin)
    //     })
    //     .collect_vec();

    let samples = mk_ite_output_samples(opts, state, &truthy.exact.samples, &falsey.exact.samples);

    let accept_l = state.mgr.and(pred_dist, truthy.exact.accept);
    let accept_r = state.mgr.and(pred_dist.neg(), falsey.exact.accept);
    let accept = state.mgr.or(accept_l, accept_r);
    let accept = state.mgr.and(accept, ctx.exact.accept);

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
        out: Some(dist),
        accept,
        samples,
        weightmap,
        substitutions,
    })
}

#[cfg(feature = "debug_samples")]
fn mk_elet_output_samples(
    opts: &Opts,
    mgr: &mut Mgr,
    bindee_samples: &HashMap<BddPtr, bool>,
    body_samples: &HashMap<BddPtr, bool>,
) -> HashMap<BddPtr, bool> {
    if !opts.sample_pruning {
        let mut samples = bindee_samples.clone();
        samples.extend(body_samples);
        return samples;
    };
    Default::default()
}
#[cfg(not(feature = "debug_samples"))]
fn mk_elet_output_samples(
    opts: &Opts,
    mgr: &mut Mgr,
    bindee_samples: &BddPtr,
    body_samples: &BddPtr,
) -> BddPtr {
    if !opts.sample_pruning {
        return mgr.and(*bindee_samples, *body_samples);
    };
    BddPtr::PtrTrue
}
pub fn eval_elet_output(
    mgr: &mut Mgr,
    ctx: &Ctx,
    bound: Output,
    body: Output,
    opts: &Opts,
) -> Result<EOutput> {
    let accept = mgr.and(body.exact.accept, ctx.exact.accept);
    let samples = mk_elet_output_samples(opts, mgr, &ctx.exact.samples, &body.exact.samples);

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
#[derive(Clone, Debug)]
pub struct Opts {
    pub sample_pruning: bool,
    pub max_label: u64,
    pub order: VarOrder,
}

pub struct State<'a> {
    pub opts: Opts,
    pub mgr: &'a mut Mgr,
    pub rng: Option<&'a mut StdRng>, // None will use the thread rng
    pub funs: &'a HashMap<FnId, Fun>,
    log_pq: LPQ,
    pub fnctx: Option<FnCall>,
}

impl<'a> State<'a> {
    pub fn new(
        mgr: &'a mut Mgr,
        rng: Option<&'a mut StdRng>, // None will use the thread rng
        sample_pruning: bool,
        funs: &'a HashMap<FnId, Fun>,
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
            log_pq: LPQ::default(),
            funs,
            fnctx: None,
        }
    }
    pub fn log_pq(&self) -> LPQ {
        self.log_pq
    }
    pub fn mult_pq(&mut self, p: f64, q: f64) {
        // if !(p == q) {
        self.log_pq.lp = self.log_pq.lp.add(LW::new(p));
        self.log_pq.lq = self.log_pq.lq.add(LW::new(q));
        // }
    }
    pub fn eval_program_with_data(
        &mut self,
        prog: &Program<Annotated>,
        dview: &DataView1,
    ) -> Result<Output> {
        match prog {
            Program::SBody(e) => {
                tracing::trace!("compiling sbody...");
                tracing::debug!("ast: {e:?}");
                let c = self.eval_sexpr(Ctx::default_with_data(dview), e)?;
                Ok(c)
            }
            Program::EBody(e) => {
                tracing::trace!("compiling ebody...");
                tracing::debug!("ast: {e:?}");
                let c = self.eval_eexpr(Ctx::default_with_data(dview), e)?;
                Ok(c)
            }
            Program::SDefine(f, e) => {
                tracing::trace!("skipping sdefine... (collected in annotate)");
                // tracing::debug!("fun: {f:?}");
                // let name = f
                //     .clone()
                //     .name
                //     .expect("all defined functions must have a name");
                // self.fns.insert(name, Fn::Sample(f.clone()));
                self.eval_program_with_data(e, dview)
            }
            Program::EDefine(f, e) => {
                tracing::trace!("skipping edefine... (collected in annotate)");
                tracing::debug!("fun: {f:?}");
                // let name = f
                //     .clone()
                //     .name
                //     .expect("all defined functions must have a name");
                // self.fns.insert(name, Fn::Exact(f.clone()));
                self.eval_program_with_data(e, dview)
            }
        }
    }

    pub fn eval_eexpr(&mut self, ctx: Ctx, e: &EExprAnn) -> Result<Output> {
        let span = tracing::span!(tracing::Level::DEBUG, "e");
        let _senter = span.enter();
        use EExpr::*;
        match e {
            EAnf(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "anf");
                let _enter = span.enter();
                tracing::debug!("anf");

                let o = eval_eanf(self, &ctx, a)?;
                debug_step_ng!("anf", ctx, &o);
                let out = ctx.mk_eoutput(o);
                Ok(out)
            }
            EFlip(d, param) => {
                let span = tracing::span!(tracing::Level::DEBUG, "flip");
                let _enter = span.enter();
                tracing::debug!("flip: {:?}", e);
                let o = eval_eanf(self, &ctx, param)?;
                tracing::debug!("flip done");
                let o = match &o.out {
                    Some(EVal::EFloat(param)) => {
                        let mut weightmap = ctx.exact.weightmap.clone();
                        weightmap.insert(d.label, *param);
                        let var = self.mgr.var(d.label, true);
                        Ok(EOutput {
                            out: Some(EVal::EBdd(var)),
                            accept: ctx.exact.accept.clone(),
                            samples: ctx.exact.samples.clone(),
                            weightmap,
                            substitutions: ctx.exact.substitutions.clone(),
                        })
                    }
                    _ => errors::typecheck_failed("flip output"),
                }?;

                debug_step_ng!("flip", ctx, o);
                let out = ctx.mk_eoutput(o);
                Ok(out)
            }
            EObserve(_, a, rest) => {
                let span = tracing::span!(tracing::Level::DEBUG, "observe");
                let _enter = span.enter();
                tracing::debug!("observe");

                let comp = eval_eanf(self, &ctx, a)?;

                // debug!("In. Accept {}", &ctx.accept.print_bdd());
                // debug!("Comp. dist {}", renderbdds(&comp.dists));
                // debug!("weightmap  {:?}", ctx.weightmap);

                // except everything up to this point
                let accept = ctx.exact.accept.clone();
                // and include any sample consistency that is necessary
                let ss = ctx.exact.samples(self.mgr, self.opts.sample_pruning);
                let accept = self.mgr.and(accept, ss);

                let dist = comp.dists().into_iter().fold(accept, |global, cur| {
                    let r = self.mgr.and(global, cur);
                    r
                });

                // let var_order = self.opts.order.clone();
                // let wmc_params = ctx.exact.weightmap.as_params(self.opts.max_label);
                // let avars = crate::utils::variables(dist);
                // for (i, var) in avars.iter().enumerate() {
                //     debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
                // }
                // debug!("WMCParams  {:?}", wmc_params);
                // debug!("VarOrder   {:?}", var_order);
                // debug!("Accept     {:?}", &dist);
                // // FIXME: should be aggregating these stats somewhere
                // debug!("using optimizations: {}", self.opts.sample_pruning);

                // // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                // // TODO Need to talk to review the semantics in detail and talk
                // // to steven about this if it doesn't get resolved. the issue is
                // // that weighting should be a /clean separation/ from exact
                // // compilation (which also saves us a WMC count) the fix is that
                // // we need to compile this at the _end_ of the program and
                // // weight the distribution accordingly. this amounts to the
                // // partially collapsed importance sampling of bayesian networks
                // // as documented in (Koller & Friedman, 2009). As-is we are kind of cheating.
                // // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                // let ss = ctx.exact.samples(self.mgr, self.opts.sample_pruning);
                // let (wmc, _) = crate::inference::calculate_wmc_prob(
                //     self.mgr,
                //     &wmc_params,
                //     &var_order,
                //     dist.clone(),
                //     ctx.exact.accept.clone(),
                //     ss,
                // );

                // self.mult_pq(wmc, 1.0);
                // // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

                let o = EOutput {
                    out: Some(EVal::EBdd(BddPtr::PtrTrue)),
                    accept: dist.clone(),
                    samples: ctx.exact.samples.clone(),
                    weightmap: ctx.exact.weightmap.clone(),
                    substitutions: ctx.exact.substitutions.clone(),
                };

                debug_step_ng!("observe", ctx, o);
                // let ctx = ctx.mk_eoutput(o);
                let newctx = Ctx::from(&ctx.mk_eoutput(o));
                self.eval_eexpr(newctx, rest)
            }
            EIte(_, cond, t, f) => {
                let span = tracing::span!(tracing::Level::DEBUG, "ite");
                let _enter = span.enter();
                tracing::debug!("ite");

                let opts = &self.opts.clone();
                let (pred_dist, (wmc_true, wmc_false)) =
                    eval_eite_predicate(self, &ctx, cond, opts)?;

                debug!("=============================");
                debug!("wmc_true {}, wmc_false {}", wmc_true, wmc_false);
                debug!("=============================");

                let tspan = tracing::span!(tracing::Level::DEBUG, "tru");
                let _tenter = tspan.enter();
                let truthy = self.eval_eexpr(ctx.clone(), t)?;
                drop(_tenter);

                let fspan = tracing::span!(tracing::Level::DEBUG, "fls");
                let _fenter = fspan.enter();
                let falsey = self.eval_eexpr(ctx.clone(), f)?;
                drop(_fenter);

                // if truthy.exact.out.is_none() != falsey.exact.out.len() {
                //     return Err(TypeError(format!("Expected both branches of ITE to return same len tuple\nGot (left): {:?}\nGot (right):{:?}", truthy.exact.out.len(), falsey.exact.out.len(),)));
                // }
                let o: EOutput = eval_eite_output(
                    self,
                    &ctx,
                    pred_dist,
                    (wmc_true, truthy.clone()),
                    (wmc_false, falsey),
                    opts,
                )?;
                debug_step_ng!("ite", ctx, &o);
                let out = ctx.mk_eoutput(o);
                Ok(out)
            }
            ELetIn(d, s, ebound, ebody) => {
                let span = tracing::span!(Level::DEBUG, "let", var = s);
                let _enter = span.enter();
                tracing::debug!("let");

                let bound = self.eval_eexpr(ctx.clone(), ebound)?;
                debug!("ebound: {:?}", bound.exact.out);

                // let mut newctx = ctx.new_from_eoutput(&bound);
                let mut newctx = Ctx::from(&bound);
                newctx.exact.substitutions.insert(
                    d.id(),
                    // Subst::mk(bound.exact.out.clone(), Some(Var::Named(d.clone()))),
                    bound.exact.out.clone().unwrap().clone(),
                );
                let body = self.eval_eexpr(newctx, ebody)?;

                let o = eval_elet_output(self.mgr, &ctx, bound.clone(), body, &self.opts)?;
                debug_step_ng!(format!("let-in {}", s), ctx, o);

                let outs = ctx.mk_eoutput(o);
                Ok(outs)
            }
            EApp(fcall, fname, args) => {
                let span = tracing::span!(Level::DEBUG, "app", f = fname);
                let _enter = span.enter();
                tracing::debug!("app");

                let f = self
                    .funs
                    .get(&fcall.0)
                    .expect("function is defined")
                    .exact()?;
                let params = f.args2vars(|v| match v {
                    Anf::AVar(nv, _) => Ok(nv.clone()),
                    _ => errors::generic(&format!(
                        "function {:?} argument definitions malformed",
                        fname
                    )),
                })?;
                let argvals = eval_eanfs(self, &ctx, args)?;
                let argvals = argvals.out.unwrap();
                match argvals {
                    EVal::EProd(argvals) => {
                        if params.len() != argvals.len() {
                            return errors::generic(&format!(
                                "function {:?} arguments mismatch\nleft: {:?}\nright{:?}",
                                fname, params, argvals
                            ));
                        }

                        let mut subs = ctx.exact.substitutions.clone();
                        for (param, val) in params.iter().zip(argvals.iter()) {
                            subs.insert(
                                param.id(),
                                // Subst::mk(vec![val.clone()], Some(Var::Named(param.clone()))),
                                val.clone(), // Subst::mk(vec![val.clone()], Some(Var::Named(param.clone()))),
                            );
                        }
                        let mut callerctx = ctx.clone();
                        callerctx.exact.substitutions = subs;

                        let out = self.eval_eexpr(callerctx, &f.body)?;
                        Ok(out)
                    }
                    _ => panic!(),
                }
            }
            EIterate(fid, fname, init, k) => {
                let span = tracing::span!(Level::DEBUG, "iterate", f = fname);
                let _enter = span.enter();
                tracing::debug!("iterate");

                let argoutput = eval_eanf(self, &ctx, init)?;
                tracing::trace!("arg: {:?}", argoutput.out);

                let mut arg = argoutput.out.clone().unwrap();

                let niters = eval_eanf(self, &ctx, k)?;
                match &niters.out {
                    Some(EVal::EInteger(0)) => {
                        let fout = ctx.mk_eoutput(argoutput);

                        Ok(fout)
                    }
                    Some(EVal::EInteger(i)) => {
                        let mut niters: usize = *i;
                        let mut ctx = ctx.clone();
                        let mut out = None;
                        debug!("niters: {}", niters);
                        let mut callix = 0;
                        while niters > 0 {
                            let anfarg = Anf::AVal((), arg.clone());
                            let o = self.eval_eexpr(
                                ctx.clone(),
                                &EApp(FnCall(*fid, callix), fname.clone(), vec![anfarg]),
                            )?;
                            out = Some(o.clone());
                            ctx = ctx.new_from_eoutput(&o.exact);
                            arg = o.exact.out.unwrap();
                            niters -= 1;
                            callix += 1;
                            debug!("niters: {}", niters);
                        }

                        let fout = out.expect("k > 0");
                        Ok(fout)
                    }
                    _ => errors::typecheck_failed("iterate k"),
                }
            }
            ESample((), sexpr) => {
                let span = tracing::span!(Level::DEBUG, "sample");
                let _enter = span.enter();
                tracing::debug!("sample");

                debug!("sample starting...");
                let mut o = self.eval_sexpr(ctx, sexpr)?;
                let sout = o.sample.out.clone().unwrap();
                debug!("sample output: {:?}", sout);
                let out = EExpr::<Trace>::embed(&sout)?;
                debug!("sample embedded {:?}", out);

                o.exact.out = Some(out);
                Ok(o)
            }
            EDiscrete(_, _) => errors::erased(Trace, "discrete"),
        }
    }
    pub fn eval_sexpr(&mut self, ctx: Ctx, e: &SExprAnn) -> Result<Output> {
        use SExpr::*;
        let span = tracing::span!(tracing::Level::DEBUG, "s");
        let _senter = span.enter();
        match e {
            SAnf(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sanf");
                let _enter = span.enter();
                debug!("ast: {:?}", e);

                let out = eval_sanf(self, &ctx, a)?;
                // let out = ctx.mk_soutput(out);
                Ok(out)
            }
            SLetIn(d, name, bindee, body) => {
                let span = tracing::span!(tracing::Level::DEBUG, "let", v = name);
                let _enter = span.enter();
                tracing::debug!("let");
                let bound = self.eval_sexpr(ctx.clone(), bindee)?; // clone ctx purely for debug
                debug!("sbound: {} = {:?}", name, bound.sample.out);

                let mut newctx = Ctx::from(&bound);
                newctx
                    .sample
                    .substitutions
                    // .insert(d.id(), Subst::mk(bound.sample.out.clone(), None));
                    .insert(d.id(), bound.sample.out.unwrap().clone()); // Subst::mk(bound.sample.out.clone(), None));
                let out = self.eval_sexpr(newctx, body)?;

                debug_step_ng!(format!("let-in {}", name), ctx, out; "sample");

                Ok(out)
            }
            SSeq(_, s0, s1) => {
                let span = tracing::span!(tracing::Level::DEBUG, "seq");
                let _enter = span.enter();
                tracing::debug!("seq");
                let s0out = self.eval_sexpr(ctx.clone(), s0)?;
                let newctx = Ctx::from(&s0out);
                let s1out = self.eval_sexpr(newctx, s1)?;
                debug_step_ng!("seq", ctx, s1out; "sample");
                Ok(s1out)
            }
            SIte(_, guard, truthy, falsey) => {
                let span = tracing::span!(tracing::Level::DEBUG, "ite");
                let _enter = span.enter();
                tracing::debug!("ite");
                let guardout = crate::compile::anf::eval_sanf(self, &ctx, guard)?;
                // Note that we are _collapsing_ SIte because the program trace does not go down both sides.
                match &guardout.sample.out {
                    Some(SVal::SBool(true)) => self.eval_sexpr(ctx.clone(), truthy),
                    Some(SVal::SBool(false)) => self.eval_sexpr(ctx.clone(), falsey),
                    _ => errors::typecheck_failed("s-ite guard"),
                }
            }
            SMap(d, argname, body, arg) => {
                panic!()
                // let span = tracing::span!(tracing::Level::DEBUG, "map");
                // let _enter = span.enter();
                // tracing::debug!("map");
                // let argval = crate::compile::anf::eval_sanf(self, &ctx, arg)?;
                // if argval.sample.out.len() != 1 {
                //     errors::typecheck_failed("map args != 1")
                // } else {
                //     let vs = match &argval.sample.out {
                //         Some(SVal::SProd(vs)) => Ok(vs),
                //         Some(SVal::SVec(vs)) => Ok(vs),
                //         _ => errors::typecheck_failed("map arg != prod or vec"),
                //     }?;
                //     let outs = vs
                //         .iter()
                //         .map(|v| {
                //             let mut newctx = Ctx::from(&argval);
                //             newctx
                //                 .sample
                //                 .substitutions
                //                 .insert(d.id(), Subst::mk(vec![v.clone()], None));
                //             let o = self.eval_sexpr(newctx, body)?;
                //             if o.sample.out.len() != 1 {
                //                 errors::typecheck_failed("map fn did not compile to a single value")
                //             } else {
                //                 Ok(o.sample.out[0].clone())
                //             }
                //         })
                //         .collect::<Result<Vec<SVal>>>()?;
                //     let sout = ctx.sample.as_output(outs);
                //     let out = ctx.mk_soutput(sout);
                //     Ok(out)
                // }
            }
            SWhile(_, guard, body) => {
                let span = tracing::span!(tracing::Level::DEBUG, "while");
                let _enter = span.enter();
                tracing::debug!("while");

                let mut loopctx = ctx.clone();
                let guardout = loop {
                    let g = crate::compile::anf::eval_sanf(self, &loopctx, guard)?;
                    match &g.sample.out {
                        Some(SVal::SBool(true)) => break Ok(g),
                        Some(SVal::SBool(false)) => {
                            let out = self.eval_sexpr(loopctx, body)?;
                            loopctx = Ctx::from(&out);
                        }
                        _ => break errors::typecheck_failed("while guard"),
                    }
                }?;
                // let out = ctx.mk_soutput(guardout);
                Ok(guardout)
            }
            SFold((accvar, argvar), initial, acc, arg, body, values) => {
                let span = tracing::span!(tracing::Level::DEBUG, "fold");
                let _enter = span.enter();
                tracing::debug!("fold");

                let valueso = crate::compile::anf::eval_sanf(self, &ctx, values)?;

                errors::TODO()
                // if valueso.out.len() != 1 {
                //     errors::typecheck_failed()
                // } else {
                //     let vs = match &valueso.out {
                //        Some(SVal::SProd(vs)) => Ok(vs),
                //        Some(SVal::SVec(vs)) => Ok(vs),
                //        _ => errors::typecheck_failed(),
                //     }?;

                //     let (initialo, initial) = crate::compile::anf::eval_sanf(&mut self, &ctx, initial)?;

                //     let mut bodytr = None;
                //     let outs = vs.iter().map(|v| {
                //         let mut newctx = Ctx::from(&ctx.mk_soutput(valueso.clone()));
                //         newctx
                //             .sample
                //             .substitutions
                //             .insert(argvar.id(), vec![v.clone()]);
                //         let (o, b) = self.eval_sexpr(newctx, body)?;
                //         bodytr = Some(b);
                //         if o.sample.out.len() != 1 {
                //             errors::typecheck_failed()
                //         } else {
                //             Ok(o.sample.out[0].clone())
                //         }
                //     }).collect::<Result<Vec<SVal>>>()?;
                //     let sout = ctx.sample.as_output(outs);
                //     let out = ctx.mk_soutput(sout);
                //     Ok((out.clone(), SMap(out, argname.clone(), Box::new(bodytr.unwrap()), Box::new(arg))))
                // }
            }
            SApp(fcall, fname, args) => {
                let span = tracing::span!(tracing::Level::DEBUG, "app");
                let _enter = span.enter();
                tracing::debug!("fold");
                let f = self
                    .funs
                    .get(&fcall.0)
                    .expect("function is defined")
                    .sample()?;
                let params = f.args2vars(|v| match v {
                    Anf::AVar(nv, _) => Ok(nv.clone()),
                    _ => errors::generic(&format!(
                        "function {:?} argument definitions malformed",
                        fname
                    )),
                })?;
                let argvals = eval_sanfs(self, &ctx, args)?;
                let argvals = argvals.sample.out.unwrap();
                match argvals {
                    SVal::SVec(argvals) => {
                        if params.len() != argvals.len() {
                            tracing::debug!("params: {params:?}");
                            tracing::debug!("argvals: {argvals:?}");
                            return errors::generic(&format!(
                                "function {:?} arguments mismatch. Got {}, expected {}",
                                fname,
                                params.len(),
                                argvals.len()
                            ));
                        }
                        let mut subs = ctx.sample.substitutions.clone();
                        for (param, val) in params.iter().zip(argvals.iter()) {
                            // subs.insert(param.id(), Subst::mk(vec![val.clone()], None));
                            subs.insert(param.id(), val.clone());
                        }
                        let mut callerctx = ctx.clone();
                        callerctx.sample.substitutions = subs;

                        let out = self.eval_sexpr(callerctx, &f.body)?;
                        Ok(out)
                    }
                    _ => panic!(),
                }
            }
            SLambda(_, _, _) => errors::TODO(),

            SSample(_, dist) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample");
                let _enter = span.enter();
                tracing::debug!("sample");
                let mut out = self.eval_sexpr(ctx, &dist)?;

                let (q, v, d) = match &out.sample.out {
                    Some(SVal::SDist(d)) => match d {
                        Dist::Bern(param) => {
                            let dist = statrs::distribution::Bernoulli::new(*param).unwrap();
                            let v = sample_from(self, &dist);
                            let q = dist.pmf(v as u64);
                            let v = SVal::SBool(v == 1.0);
                            (q, v, d)
                        }
                        Dist::Discrete(ps) => {
                            let dist = statrs::distribution::Categorical::new(&ps).unwrap();
                            let v = sample_from(self, &dist);
                            let q = dist.pmf(v as u64);
                            let v = SVal::SInt(v as u64);
                            (q, v, d)
                        }
                        Dist::Dirichlet(ps) => {
                            let dist = statrs::distribution::Dirichlet::new(ps.to_vec()).unwrap();
                            let vs = sample_from(self, &dist);
                            let q = dist.pdf(&vs);
                            println!("{:?}", vs);
                            let vs = SVal::SVec(
                                vs.into_iter().map(|x: &f64| SVal::SFloat(*x)).collect_vec(),
                            );
                            (q, vs, d)
                        }
                        Dist::Uniform(lo, hi) => {
                            let dist = statrs::distribution::Uniform::new(*lo, *hi).unwrap();
                            let v = sample_from(self, dist);
                            let q = dist.pdf(v);
                            let v = SVal::SFloat(v);
                            (q, v, d)
                        }
                        Dist::Normal(mn, sd) => {
                            let dist = statrs::distribution::Normal::new(*mn, *sd).unwrap();
                            let v = sample_from(self, dist);
                            let q = dist.pdf(v);
                            let v = SVal::SFloat(v);
                            (q, v, d)
                        }
                        Dist::Beta(a, b) => {
                            let dist = statrs::distribution::Beta::new(*a, *b).unwrap();
                            let v = sample_from(self, dist);
                            let q = dist.pdf(v);
                            let v = SVal::SFloat(v);
                            (q, v, d)
                        }
                        Dist::Poisson(p) => {
                            let dist = statrs::distribution::Poisson::new(*p).unwrap();
                            let v = sample_from(self, dist) as u64;
                            let q = dist.pmf(v);
                            let v = SVal::SInt(v);
                            (q, v, d)
                        }
                    },
                    _ => panic!("semantic error, see note on SObserve in SExpr enum"),
                };
                trace!("  dist: {:?}", d);
                trace!("sample: {:?}", v);
                trace!("  prob: {}", q);

                out.sample.trace.push((v.clone(), d.clone(), q, None));
                out.sample.out = Some(v);
                Ok(out)
            }

            SObserve(_, a, dist, rst) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample-observe");
                let _enter = span.enter();
                tracing::debug!("observe");

                let a_out = crate::compile::anf::eval_sanf(self, &ctx, a)?;
                let d_out = crate::compile::anf::eval_sanf(self, &ctx, dist)?;
                let q = match (&d_out.sample.out, &a_out.sample.out) {
                    (Some(SVal::SDist(d)), Some(v)) => match (d, v) {
                        (Dist::Bern(param), SVal::SBool(b)) => {
                            let dist = statrs::distribution::Bernoulli::new(*param).unwrap();
                            statrs::distribution::Discrete::pmf(&dist, *b as u64)
                        }
                        (Dist::Discrete(ps), SVal::SInt(i)) => {
                            let dist = statrs::distribution::Categorical::new(&ps).unwrap();
                            statrs::distribution::Discrete::pmf(&dist, *i as u64)
                        }
                        (Dist::Dirichlet(ps), _) => todo!("punted"),
                        // FIXME: Punt-- might need a different library to preform this integration.
                        // (Dist::Dirichlet(ps), SVal::SVec(vs)) => {
                        //     let dist = statrs::distribution::Dirichlet::new(ps.to_vec()).unwrap();
                        //     let vs = vs
                        //         .iter()
                        //         .map(|v| match v {
                        //             SVal::SFloat(f) => Ok(*f),
                        //             _ => errors::typecheck_failed("observe dirichlet"),
                        //         })
                        //         .collect::<Result<Vec<f64>>>()?;
                        //     let vs = <nalgebra::base::DVector<f64> as From<Vec<f64>>>::from(vs);

                        //     statrs::distribution::ContinuousCDF::cdf(&dist, &vs)
                        // }
                        (Dist::Uniform(lo, hi), SVal::SFloat(f)) => {
                            let dist = statrs::distribution::Uniform::new(*lo, *hi).unwrap();
                            statrs::distribution::ContinuousCDF::cdf(&dist, *f)
                        }
                        (Dist::Normal(mn, sd), SVal::SFloat(f)) => {
                            let dist = statrs::distribution::Normal::new(*mn, *sd).unwrap();
                            statrs::distribution::ContinuousCDF::cdf(&dist, *f)
                        }
                        (Dist::Beta(a, b), SVal::SFloat(f)) => {
                            let dist = statrs::distribution::Beta::new(*a, *b).unwrap();
                            statrs::distribution::ContinuousCDF::cdf(&dist, *f)
                        }
                        (d, v) => {
                            panic!("new distribution encountered in sobserve: {d:?} in {v:?}")
                        }
                    },
                    _ => panic!("semantic error, see note on SObserve in SExpr enum"),
                };
                self.mult_pq(q, 1.0);

                let newctx = Ctx::from(&a_out);
                self.eval_sexpr(newctx, rst)
            }
            // Multi-language boundary / natural embedding.
            SExact(_, eexpr) => {
                let span = tracing::span!(tracing::Level::DEBUG, "exact");
                let _enter = span.enter();
                tracing::debug!("exact");

                let mut out = self.eval_eexpr(ctx.clone(), eexpr)?;
                let eval = out.exact.out.clone().unwrap().clone();
                let val = match (&eval, SExpr::<Trace>::embed(&eval)) {
                    (_, Ok(sv)) => sv,
                    (EVal::EBdd(dist), Err(_)) => {
                        let sample = exact2sample_bdd_eff(self, &mut out, dist);
                        SVal::SBool(sample)
                    }
                    // best effort for prods
                    (EVal::EProd(vs), Err(e)) => SVal::SProd(
                        vs.iter()
                            .map(|v| {
                                Ok(match (&v, SExpr::<Trace>::embed(&v)) {
                                    (EVal::EBdd(dist), Err(_)) => {
                                        let sample = exact2sample_bdd_eff(self, &mut out, dist);
                                        SVal::SBool(sample)
                                    }
                                    (_, Ok(v)) => v,
                                    (_, Err(e)) => return Err(e),
                                })
                            })
                            .collect::<Result<Vec<_>>>()?,
                    ),
                    (_, Err(e)) => return Err(e),
                };
                trace!("boundary sample: {:?}", val);
                out.sample.out = Some(val);
                Ok(out)
            }
            SLetSample(_, _, _, _) => errors::erased(Trace, "let-sample"),
        }
    }
}
