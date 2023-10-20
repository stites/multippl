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
    let dists = izip!(&truthy.exact.dists(), &falsey.exact.dists())
        .map(|(tdist, fdist)| {
            let dist_l = state.mgr.and(pred_dist, *tdist);
            let dist_r = state.mgr.and(pred_dist.neg(), *fdist);
            let fin = state.mgr.or(dist_l, dist_r);
            EVal::EBdd(fin)
        })
        .collect_vec();

    let tsamples = GetSamples::samples(&truthy.exact, state.mgr, opts.sample_pruning);
    let mut samples = HashMap::new();
    if !opts.sample_pruning {
        samples.extend(truthy.exact.samples);
        samples.extend(falsey.exact.samples);
    };

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
    let accept = mgr.and(body.exact.accept, ctx.exact.accept);
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
    pub pq: PQ,
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
            pq: Default::default(),
            funs,
            fnctx: None,
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
    pub fn mult_pq(&mut self, p: f64, q: f64) {
        self.pq.p *= p;
        self.pq.q *= q;
    }
    pub fn eval_program(&mut self, prog: &Program<Annotated>) -> Result<Output> {
        match prog {
            Program::SBody(e) => {
                tracing::trace!("compiling sbody...");
                tracing::debug!("ast: {e:?}");
                let c = self.eval_sexpr(Ctx::default(), e)?;
                Ok(c)
            }
            Program::EBody(e) => {
                tracing::trace!("compiling ebody...");
                tracing::debug!("ast: {e:?}");
                let c = self.eval_eexpr(Ctx::default(), e)?;
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
                self.eval_program(e)
            }
            Program::EDefine(f, e) => {
                tracing::trace!("skipping edefine... (collected in annotate)");
                tracing::debug!("fun: {f:?}");
                // let name = f
                //     .clone()
                //     .name
                //     .expect("all defined functions must have a name");
                // self.fns.insert(name, Fn::Exact(f.clone()));
                self.eval_program(e)
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
                let o = match &o.out[..] {
                    [EVal::EFloat(param)] => {
                        let mut weightmap = ctx.exact.weightmap.clone();
                        weightmap.insert(d.label, *param);
                        let var = self.mgr.var(d.label, true);
                        Ok(EOutput {
                            out: vec![EVal::EBdd(var)],
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
            EObserve(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "observe");
                let _enter = span.enter();
                tracing::debug!("observe");

                let comp = eval_eanf(self, &ctx, a)?;

                // debug!("In. Accept {}", &ctx.accept.print_bdd());
                // debug!("Comp. dist {}", renderbdds(&comp.dists));
                // debug!("weightmap  {:?}", ctx.weightmap);
                let dist = comp
                    .dists()
                    .into_iter()
                    .fold(ctx.exact.accept.clone(), |global, cur| {
                        self.mgr.and(global, cur)
                    });

                let var_order = self.opts.order.clone();
                let wmc_params = ctx.exact.weightmap.as_params(self.opts.max_label);
                let avars = crate::utils::variables(dist);
                for (i, var) in avars.iter().enumerate() {
                    debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
                }
                debug!("WMCParams  {:?}", wmc_params);
                debug!("VarOrder   {:?}", var_order);
                debug!("Accept     {:?}", &dist);
                // FIXME: should be aggregating these stats somewhere
                debug!("using optimizations: {}", self.opts.sample_pruning);

                // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                // TODO Need to talk to review the semantics in detail and talk
                // to steven about this if it doesn't get resolved. the issue is
                // that weighting should be a /clean separation/ from exact
                // compilation (which also saves us a WMC count) the fix is that
                // we need to compile this at the _end_ of the program and
                // weight the distribution accordingly. this amounts to the
                // partially collapsed importance sampling of bayesian networks
                // as documented in (Koller & Friedman, 2009). As-is we are kind of cheating.
                // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                let ss = ctx.exact.samples(self.mgr, self.opts.sample_pruning);
                let (wmc, _) = crate::inference::calculate_wmc_prob(
                    self.mgr,
                    &wmc_params,
                    &var_order,
                    dist.clone(),
                    ctx.exact.accept.clone(),
                    ss,
                );
                info!("  dist: {}", dist.print_bdd());
                info!("accept: {}", ctx.exact.accept.print_bdd());
                info!("   map: {:?}", wmc_params);
                info!("   wmc: {wmc}");
                self.pq.p *= wmc;
                // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

                let o = EOutput {
                    out: vec![EVal::EBdd(BddPtr::PtrTrue)],
                    accept: dist.clone(),
                    samples: ctx.exact.samples.clone(),
                    weightmap: ctx.exact.weightmap.clone(),
                    substitutions: ctx.exact.substitutions.clone(),
                };

                debug_step_ng!("observe", ctx, o);
                let out = ctx.mk_eoutput(o);
                Ok(out)
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

                if truthy.exact.out.len() != falsey.exact.out.len() {
                    return Err(TypeError(format!("Expected both branches of ITE to return same len tuple\nGot (left): {:?}\nGot (right):{:?}", truthy.exact.out.len(), falsey.exact.out.len(),)));
                }
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

                // let mut newctx = ctx.new_from_eoutput(&bound);
                let mut newctx = Ctx::from(&bound);
                newctx.exact.substitutions.insert(
                    d.id(),
                    Subst::mk(bound.exact.out.clone(), Some(Var::Named(d.clone()))),
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
                let argvals = argvals.out;
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
                        Subst::mk(vec![val.clone()], Some(Var::Named(param.clone()))),
                    );
                }
                let mut callerctx = ctx.clone();
                callerctx.exact.substitutions = subs;

                let out = self.eval_eexpr(callerctx, &f.body)?;
                Ok(out)
            }
            EIterate(fid, fname, init, k) => {
                let span = tracing::span!(Level::DEBUG, "iterate", f = fname);
                let _enter = span.enter();
                tracing::debug!("iterate");

                let argoutput = eval_eanf(self, &ctx, init)?;
                tracing::trace!("arg: {:?}", argoutput.out);

                // HACK: currently we treat everything as a prod. This should
                // probably be changed back. Here is a good example of when we
                // do _not_ want this generalization:
                let mut arg = if argoutput.out.len() == 1 {
                    argoutput.out[0].clone()
                } else {
                    EVal::EProd(argoutput.out.clone())
                };

                let niters = eval_eanf(self, &ctx, k)?;
                match &niters.out[..] {
                    [EVal::EInteger(0)] => {
                        let fout = ctx.mk_eoutput(argoutput);

                        Ok(fout)
                    }
                    [EVal::EInteger(i)] => {
                        let mut niters: usize = *i;
                        let mut ctx = ctx.clone();
                        let mut out = None;
                        trace!("niters: {}", niters);
                        let mut callix = 0;
                        while niters > 0 {
                            let anfarg = Anf::AVal((), arg.clone());
                            let o = self
                                .eval_eexpr(
                                    ctx.clone(),
                                    &EApp(FnCall(*fid, callix), fname.clone(), vec![anfarg]),
                                )?
                                ;
                            out = Some(o.clone());
                            ctx = ctx.new_from_eoutput(&o.exact);
                            arg = if o.exact.out.len() == 1 {
                                o.exact.out[0].clone()
                            } else {
                                EVal::EProd(o.exact.out)
                            };
                            niters -= 1;
                            callix += 1;
                            trace!("niters: {}", niters);
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

                let mut o = self.eval_sexpr(ctx, sexpr)?;
                let sout = o.sample.out.clone();

                let out = sout
                    .iter()
                    .map(EExpr::<Trace>::embed)
                    .collect::<Result<Vec<_>>>()?;
                o.exact.out = out;
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
                debug!("anf: {:?}", a);
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

                let mut newctx = Ctx::from(&bound);
                newctx
                    .sample
                    .substitutions
                    .insert(d.id(), Subst::mk(bound.sample.out.clone(), None));
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
                match &guardout.sample.out[..] {
                    [SVal::SBool(true)] => self.eval_sexpr(ctx.clone(), truthy),
                    [SVal::SBool(false)] => self.eval_sexpr(ctx.clone(), falsey),
                    _ => errors::typecheck_failed("s-ite guard"),
                }
            }
            SMap(d, argname, body, arg) => {
                let span = tracing::span!(tracing::Level::DEBUG, "map");
                let _enter = span.enter();
                tracing::debug!("map");
                let argval = crate::compile::anf::eval_sanf(self, &ctx, arg)?;
                if argval.sample.out.len() != 1 {
                    errors::typecheck_failed("map args != 1")
                } else {
                    let vs = match &argval.sample.out[..] {
                        [SVal::SProd(vs)] => Ok(vs),
                        [SVal::SVec(vs)] => Ok(vs),
                        _ => errors::typecheck_failed("map arg != prod or vec"),
                    }?;
                    let outs = vs
                        .iter()
                        .map(|v| {
                            let mut newctx = Ctx::from(&argval);
                            newctx
                                .sample
                                .substitutions
                                .insert(d.id(), Subst::mk(vec![v.clone()], None));
                            let o = self.eval_sexpr(newctx, body)?;
                            if o.sample.out.len() != 1 {
                                errors::typecheck_failed("map fn did not compile to a single value")
                            } else {
                                Ok(o.sample.out[0].clone())
                            }
                        })
                        .collect::<Result<Vec<SVal>>>()?;
                    let sout = ctx.sample.as_output(outs);
                    let out = ctx.mk_soutput(sout);
                    Ok(out)
                }
            }
            SWhile(_, guard, body) => {
                let span = tracing::span!(tracing::Level::DEBUG, "while");
                let _enter = span.enter();
                tracing::debug!("while");

                let mut loopctx = ctx.clone();
                let guardout = loop {
                    let g = crate::compile::anf::eval_sanf(self, &loopctx, guard)?;
                    match &g.sample.out[..] {
                        [SVal::SBool(true)] => break Ok(g),
                        [SVal::SBool(false)] => {
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
                //     let vs = match &valueso.out[..] {
                //        [SVal::SProd(vs)] => Ok(vs),
                //        [SVal::SVec(vs)] => Ok(vs),
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
                let argvals = argvals.sample.out;
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
                    subs.insert(param.id(), Subst::mk(vec![val.clone()], None));
                }
                let mut callerctx = ctx.clone();
                callerctx.sample.substitutions = subs;

                let out = self.eval_sexpr(callerctx, &f.body)?;
                Ok(out)
            }
            SLambda(_, _, _) => errors::TODO(),

            SSample(_, dist) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample");
                let _enter = span.enter();
                tracing::debug!("sample");
                let mut out = self.eval_sexpr(ctx, &dist)?;

                let (q, v, d) = match &out.sample.out[..] {
                    [SVal::SDist(d)] => match d {
                        Dist::Bern(param) => {
                            let dist = statrs::distribution::Bernoulli::new(*param).unwrap();
                            let v = sample_from(self, &dist);
                            let q = statrs::distribution::Discrete::pmf(&dist, v as u64);
                            let v = SVal::SBool(v == 1.0);
                            (q, v, d)
                        }
                        Dist::Discrete(ps) => {
                            let dist = statrs::distribution::Categorical::new(&ps).unwrap();
                            let v = sample_from(self, &dist);
                            let q = statrs::distribution::Discrete::pmf(&dist, v as u64);
                            let v = SVal::SInt(v as u64);
                            (q, v, d)
                        }
                        Dist::Dirichlet(ps) => todo!("punted"),
                        // Dist::Dirichlet(ps) => {
                        //     let dist = statrs::distribution::Dirichlet::new(ps.to_vec()).unwrap();
                        //     let vs = sample_from(self, &dist);
                        //     let q = statrs::distribution::ContinuousCDF::cdf(&dist, &vs);
                        //     let vs = SVal::SVec(
                        //         vs.into_iter().map(|x: &f64| SVal::SFloat(*x)).collect_vec(),
                        //     );
                        //     (q, vs, d)
                        // }
                        Dist::Uniform(lo, hi) => {
                            let dist = statrs::distribution::Uniform::new(*lo, *hi).unwrap();
                            let v = sample_from(self, dist);
                            let q = statrs::distribution::ContinuousCDF::cdf(&dist, v);
                            let v = SVal::SFloat(v);
                            (q, v, d)
                        }
                        Dist::Normal(mn, sd) => {
                            let dist = statrs::distribution::Normal::new(*mn, *sd).unwrap();
                            let v = sample_from(self, dist);
                            let q = statrs::distribution::ContinuousCDF::cdf(&dist, v);
                            let v = SVal::SFloat(v);
                            (q, v, d)
                        }
                        Dist::Beta(a, b) => {
                            let dist = statrs::distribution::Beta::new(*a, *b).unwrap();
                            let v = sample_from(self, dist);
                            let q = statrs::distribution::ContinuousCDF::cdf(&dist, v);
                            let v = SVal::SFloat(v);
                            (q, v, d)
                        }
                        Dist::Poisson(p) => {
                            let dist = statrs::distribution::Poisson::new(*p).unwrap();
                            let v = sample_from(self, dist) as u64;
                            let q = statrs::distribution::Discrete::pmf(&dist, v);
                            let v = SVal::SInt(v);
                            (q, v, d)
                        }
                    },
                    _ => panic!("semantic error, see note on SObserve in SExpr enum"),
                };
                info!("  dist: {:?}", d);
                info!("sample: {:?}", v);
                info!("  prob: {}", q);
                self.mult_pq(q, q);

                out.sample
                    .trace
                    .push((v.clone(), d.clone(), Probability::new(q), None));
                out.sample.out = vec![v];
                Ok(out)
            }

            SObserve(_, a, dist) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample-observe");
                let _enter = span.enter();
                tracing::debug!("observe");

                let a_out = crate::compile::anf::eval_sanf(self, &ctx, a)?;
                let d_out = crate::compile::anf::eval_sanf(self, &ctx, dist)?;
                let q = match (&d_out.sample.out[..], &a_out.sample.out[..]) {
                    ([SVal::SDist(d)], [v]) => match (d, v) {
                        (Dist::Bern(param), SVal::SBool(b)) => {
                            let dist = statrs::distribution::Bernoulli::new(*param).unwrap();
                            statrs::distribution::Discrete::pmf(&dist, *b as u64)
                        }
                        (Dist::Discrete(ps), SVal::SInt(i)) => {
                            let dist = statrs::distribution::Categorical::new(&ps).unwrap();
                            statrs::distribution::Discrete::pmf(&dist, *i as u64)
                        }
                        (Dist::Dirichlet(ps), _) => todo!("punted"),
                        // FIXME: Punt-- might nee a different library to preform this integration.
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
                self.mult_pq(1.0, q);

                //let a_out = ctx.mk_soutput(a_out);
                Ok(a_out)
            }
            // Multi-language boundary / natural embedding.
            SExact(_, eexpr) => {
                let span = tracing::span!(tracing::Level::DEBUG, "exact");
                let _enter = span.enter();
                tracing::debug!("exact");

                let mut out = self.eval_eexpr(ctx.clone(), eexpr)?;
                let eouts = out.exact.out.clone();

                for eval in eouts.iter() {
                    let val = match (eval, SExpr::<Trace>::embed(eval)) {
                        (_, Ok(sv)) => sv,
                        (EVal::EBdd(dist), Err(_)) => {
                            let sample = exact2sample_bdd_eff(self, &mut out, dist);
                            SVal::SBool(sample)
                        }
                        _ => panic!("typecheck_failed"),
                    };
                    out.sample.out.push(val);
                }
                info!("boundary sample: {:?}", out.sample.out);
                Ok(out)
            }
            SLetSample(_, _, _, _) => errors::erased(Trace, "let-sample"),
        }
    }
}
