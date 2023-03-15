pub mod compiled;
pub mod context;
pub mod errors;
pub mod importance;
pub mod weighting;

use crate::analysis::grammar::*;
use crate::annotate::grammar::*;
use crate::grammar::*;
use crate::render::*;
use crate::uniquify::grammar::UniqueId;
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

pub type Mgr = BddManager<AllTable<BddPtr>>;

pub use crate::compile::compiled::{Compiled, Output, SubstMap};
pub use crate::compile::context::Context;
pub use crate::compile::errors::{CompileError, Result};
pub use crate::compile::importance::{Importance, I};
pub use crate::compile::weighting::{Weight, WeightMap};
use CompileError::*;

pub mod grammar {
    use super::*;
    use std::fmt;
    use std::fmt::*;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Trace;

    impl ξ<Trace> for AVarExt {
        type Ext = Box<Output>;
    }
    impl ξ<Trace> for AValExt {
        type Ext = Box<Output>;
    }
    pub type AnfTr = Anf<Trace>;

    impl ξ<Trace> for EAnfExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for EFstExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for ESndExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for EPrjExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for EProdExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for ELetInExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for EIteExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for EFlipExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for EObserveExt {
        type Ext = Box<Compiled>;
    }
    impl ξ<Trace> for ESampleExt {
        type Ext = Box<Compiled>;
    }

    pub type ExprTr = Expr<Trace>;
    pub type ProgramTr = Program<Trace>;
}
use grammar::*;

#[derive(Clone, Debug)]
pub enum SamplingContext {
    Unset,
    Set(DecoratedVar),
    Sampling(DecoratedVar),
    SamplingWithLet(DecoratedVar, DecoratedVar),
}
impl SamplingContext {
    pub fn as_option(&self) -> Option<DecoratedVar> {
        use SamplingContext::*;
        match self {
            Unset => None,
            Set(v) | Sampling(v) => Some(v.clone()),
            SamplingWithLet(v, _) => Some(v.clone()),
        }
    }
}
pub struct Env<'a> {
    pub mgr: &'a mut BddManager<AllTable<BddPtr>>,
    pub rng: Option<&'a mut StdRng>, // None implies "debug mode"

    // pre-computed
    pub order: VarOrder,
    pub max_label: u64,

    // in progress
    pub sampling_context: SamplingContext,
    pub sample_pruning: bool,
    pub inv: HashMap<NamedVar, HashSet<BddVar>>,
    pub above_below: HashMap<Var, (HashSet<Var>, HashSet<Var>)>,

    // static
    pub varmap: Option<HashMap<UniqueId, Var>>,

    // ignored
    pub weightmap: Option<WmcParams<RealSemiring>>,
}
impl<'a> Env<'a> {
    pub fn new(
        mgr: &'a mut BddManager<AllTable<BddPtr>>,
        rng: Option<&'a mut StdRng>,
        sample_pruning: bool,
        inv: HashMap<NamedVar, HashSet<BddVar>>,
        above_below: HashMap<Var, (HashSet<Var>, HashSet<Var>)>,
    ) -> Env<'a> {
        Env {
            order: mgr.get_order().clone(),
            weightmap: None,
            varmap: None,
            max_label: mgr.get_order().num_vars() as u64,
            inv,
            mgr,
            rng,
            sampling_context: SamplingContext::Unset,
            above_below,
            sample_pruning,
        }
    }

    pub fn eval_anf_binop(
        &mut self,
        ctx: &Context,
        bl: &AnfAnlys,
        br: &AnfAnlys,
        op: &dyn Fn(&mut Mgr, BddPtr, BddPtr) -> BddPtr,
    ) -> Result<(AnfTr, AnfTr, Output)> {
        let (l, ltr) = self.eval_anf(ctx, bl)?;
        let (r, rtr) = self.eval_anf(ctx, br)?;

        if l.dists.len() != r.dists.len() {
            return Err(SemanticsError(format!(
                "impossible! compiled {} dists on the left and {} formulas on the right.",
                l.dists.len(),
                r.dists.len()
            )));
        } else {
            let dists = izip!(l.dists, r.dists)
                .map(|(l, r)| op(self.mgr, l, r))
                .collect_vec();

            let dists_len = dists.len();
            Ok((ltr, rtr, Output::from_anf_dists(ctx, dists)))
        }
    }

    pub fn eval_anf(&mut self, ctx: &Context, a: &AnfAnlys) -> Result<(Output, AnfTr)> {
        use Anf::*;
        match a {
            AVar(d, s) => match ctx.substitutions.get(&d.id()) {
                None => Err(Generic(format!(
                    "variable {} does not reference known substitution",
                    s
                ))),
                Some((subs, subvar)) => {
                    let c = Output::from_anf_dists(ctx, subs.to_vec());
                    Ok((c.clone(), AVar(Box::new(c), s.to_string())))
                }
            },
            AVal(_, Val::Bool(b)) => {
                let c = Output::from_anf_dists(ctx, vec![BddPtr::from_bool(*b)]);
                Ok((c.clone(), AVal(Box::new(c), Val::Bool(*b))))
            }
            AVal(_, Val::Prod(vs)) => Err(CompileError::Todo()),
            And(bl, br) => {
                let (ltr, rtr, o) = self.eval_anf_binop(ctx, bl, br, &BddManager::and)?;
                Ok((o, And(Box::new(ltr), Box::new(rtr))))
            }
            Or(bl, br) => {
                let (ltr, rtr, o) = self.eval_anf_binop(ctx, bl, br, &BddManager::or)?;
                Ok((o, Or(Box::new(ltr), Box::new(rtr))))
            }
            Neg(bp) => {
                let (mut p, ptr) = self.eval_anf(ctx, bp)?;
                // FIXME negating a tuple? seems weird!!!!
                p.dists = p.dists.iter().map(BddPtr::neg).collect_vec();
                Ok((p, Neg(Box::new(ptr))))
            }
        }
    }
    pub fn result_from(&self, c: Output) -> Result<Compiled> {
        Ok(match self.rng {
            Some(_) => Compiled::from_output(c),
            None => Compiled::Output(c),
        })
    }
    pub fn eval_expr(&mut self, ctx: &Context, e: &ExprAnlys) -> Result<(Compiled, ExprTr)> {
        use Expr::*;
        match e {
            EAnf(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "anf");
                let _enter = span.enter();
                debug!("{:?}", a);
                let (o, atr) = self.eval_anf(ctx, a)?;
                debug_step!("anf", ctx, &o);
                let c = Compiled::Output(o);
                Ok((c.clone(), EAnf(Box::new(c), Box::new(atr))))
            }
            EPrj(_, i, a) => {
                if i > &1 {
                    let span = tracing::span!(tracing::Level::DEBUG, "prj", i);
                    let _enter = span.enter();
                    debug!("{:?}", a);
                }
                let (mut o, atr) = self.eval_anf(ctx, a)?;
                let dists = o.dists;
                o.dists = vec![dists[*i]];
                if i > &1 {
                    debug_step!(&format!("prj@{}", i), ctx, o);
                }
                let c = Compiled::Output(o);
                Ok((c.clone(), EAnf(Box::new(c), Box::new(atr))))
            }
            EFst(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "fst");
                let _enter = span.enter();
                debug!("{:?}", a);
                let c = self.eval_expr(ctx, &EPrj((), 0, a.clone()))?;
                Ok(c)
            }
            ESnd(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "snd");
                let _enter = span.enter();
                debug!("{:?}", a);
                let c = self.eval_expr(ctx, &EPrj((), 1, a.clone()))?;
                Ok(c)
            }
            EProd(_, anfs) => {
                let span = tracing::span!(tracing::Level::DEBUG, "prod");
                let _enter = span.enter();
                debug!("{:?}", anfs);
                let (dists, atrs) = anfs.iter().fold(Ok((vec![], vec![])), |res, a| {
                    let (distsfin, mut atrs_fin) = res?;
                    let (o, atr) = self.eval_anf(ctx, a)?;
                    let dists = distsfin.iter().chain(&o.dists).cloned().collect_vec();
                    atrs_fin.push(atr);
                    Ok((dists, atrs_fin))
                })?;
                let flen = dists.len();
                let o = Output {
                    dists,
                    accept: ctx.accept,
                    samples: ctx.samples,
                    samples_opt: ctx.samples_opt.clone(),
                    weightmap: ctx.weightmap.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0); flen],
                    importance: I::Weight(1.0),
                };

                debug_step!("prod", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), EProd(Box::new(c), atrs)))
            }
            ELetIn(d, s, ebound, ebody) => {
                use SamplingContext::*;
                let let_in_span = tracing::span!(Level::DEBUG, "let", var = s);
                let _enter = let_in_span.enter();
                match &self.sampling_context {
                    Unset | Set(_) => {
                        self.sampling_context = Set(d.clone());
                    }
                    Sampling(v) => {
                        self.sampling_context = SamplingWithLet(v.clone(), d.clone());
                    }
                    SamplingWithLet(v, _) => {
                        self.sampling_context = SamplingWithLet(v.clone(), d.clone());
                    }
                }

                // let lbl = d.var.label;
                // if we produce multiple worlds, we must account for them all
                let (cbound, eboundtr) = self.eval_expr(ctx, ebound)?;
                let (outs, mbody) = cbound
                    .into_iter()
                    .enumerate()
                    .map(|(ix, bound)| {
                        let span = if ix == 0 {
                            tracing::span!(tracing::Level::DEBUG, "")
                        } else {
                            tracing::span!(tracing::Level::DEBUG, "", ix)
                        };
                        let _enter = span.enter();

                        let ix_span = tracing::span!(Level::DEBUG, "", ix);
                        let _enter = ix_span.enter();
                        let mut newctx = Context::from_compiled(&bound);
                        newctx
                            .substitutions
                            .insert(d.id(), (bound.dists.clone(), d.var().clone()));

                        let (bodies, bodiestr) = self.eval_expr(&newctx, ebody)?;
                        let cbodies = bodies
                            .into_iter()
                            .enumerate()
                            .map(|(ix, body)| {
                                let span = if ix == 0 {
                                    tracing::span!(tracing::Level::DEBUG, "")
                                } else {
                                    tracing::span!(tracing::Level::DEBUG, "", ix)
                                };
                                let _enter = span.enter();

                                let accept = self.mgr.and(body.accept, ctx.accept);
                                let samples;
                                let samples_opt;

                                if self.sample_pruning {
                                    samples = ctx.samples;
                                    samples_opt = body.samples_opt;
                                } else {
                                    samples = self.mgr.and(body.samples, ctx.samples);
                                    // TODO see if I need to include both body and ctx samples.
                                    samples_opt = body.samples_opt.clone();
                                }
                                let probabilities =
                                    izip!(bound.probabilities.clone(), body.probabilities)
                                        .map(|(p1, p2)| p1 * p2)
                                        .collect_vec();
                                let importance =
                                    I::Weight(bound.importance.weight() * body.importance.weight());

                                let c = Output {
                                    dists: body.dists,
                                    accept,
                                    samples,
                                    samples_opt,
                                    substitutions: body.substitutions.clone(),
                                    weightmap: body.weightmap,
                                    probabilities,
                                    importance,
                                };
                                debug_step!(format!("let-in {}", s), ctx, c);
                                Ok(c)
                            })
                            .collect::<Result<Vec<Output>>>()?;
                        Ok((cbodies, bodiestr))
                    })
                    .collect::<Result<Vec<(Vec<Output>, ExprTr)>>>()?
                    .into_iter()
                    .fold(
                        (vec![], None),
                        |(mut outs, mbody), (compiled_outs, body)| {
                            outs.extend(compiled_outs);
                            (outs, Some(body))
                        },
                    );
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
            EIte(_, cond, t, f) => {
                let span = tracing::span!(tracing::Level::DEBUG, "ite");
                let _enter = span.enter();

                let (pred, atr) = self.eval_anf(ctx, cond)?;
                if !pred.dists.len() == 1 {
                    return Err(TypeError(format!(
                        "Expected Bool for ITE condition\nGot: {cond:?}\n{ctx:?}",
                    )));
                }
                let pred_dist = pred.dists[0];
                let var_order = self.order.clone();
                let wmc_params = ctx.weightmap.as_params(self.max_label);
                let wmc_true;
                let wmc_false;

                // FIXME : should be adding stats somewhere
                if self.sample_pruning {
                    let mut wmc_opt_h = |pred_dist| {
                        Probability::new(
                            crate::inference::calculate_wmc_prob_opt(
                                self.mgr,
                                &wmc_params,
                                &var_order,
                                pred_dist,
                                ctx.accept,
                                &ctx.samples_opt,
                                &self.inv,
                                &self.above_below,
                            )
                            .0,
                        )
                    };
                    wmc_true = wmc_opt_h(pred_dist);
                    wmc_false = wmc_opt_h(pred_dist.neg());
                } else {
                    let mut wmc_h = |pred_dist| {
                        Probability::new(
                            crate::inference::calculate_wmc_prob(
                                self.mgr,
                                &wmc_params,
                                &var_order,
                                pred_dist,
                                ctx.accept,
                                ctx.samples, // TODO if switching to samples_opt, no need to use ctx.
                            )
                            .0,
                        )
                    };
                    wmc_true = wmc_h(pred_dist);
                    wmc_false = wmc_h(pred_dist.neg());
                };
                debug!("=============================");
                debug!("wmc_true {}, wmc_false {}", wmc_true, wmc_false);
                debug!("=============================");

                let span = tracing::span!(tracing::Level::DEBUG, "truthy");
                let _enter = span.enter();
                let (ct, ttr) = self.eval_expr(ctx, t)?;
                drop(_enter);
                let (c, mftr) = ct.into_iter()
                    .enumerate()
                    .map(|(ix, truthy)| {
                        let span = if ix == 0 {
                            tracing::span!(tracing::Level::DEBUG, "truthy")
                        } else {
                            tracing::span!(tracing::Level::DEBUG, "truthy", ix)
                        };
                        let _enter = span.enter();

                        let (cf, ftr) = self.eval_expr(ctx, f)?;
                        let c = cf.into_iter().enumerate().map(|(ix, falsey)| {
                            let span = if ix == 0 {
                                tracing::span!(tracing::Level::DEBUG, "falsey")
                            } else {
                                tracing::span!(tracing::Level::DEBUG, "falsey", ix)
                            };
                            let _enter = span.enter();

                            if truthy.dists.len() != falsey.dists.len() {
                                return Err(TypeError(format!("Expected both branches of ITE to return same len tuple\nGot (left): {:?}\nGot (right):{:?}", truthy.dists.len(), falsey.dists.len(),)));
                            }
                            let dists = izip!(&truthy.dists, &falsey.dists)
                                  .map(|(tdist, fdist)| {
                                      let dist_l = self.mgr.and(pred_dist, *tdist);
                                      let dist_r = self.mgr.and(pred_dist.neg(), *fdist);
                                      self.mgr.or(dist_l, dist_r)
                                  })
                                  .collect_vec();

                            let samples; let samples_opt;
                            if self.sample_pruning {
                                samples = truthy.samples;
                                samples_opt = falsey.samples_opt;
                            } else {
                                samples = self.mgr.and(truthy.samples, falsey.samples);
                                let mut samples_opt_m = truthy.samples_opt.clone();
                                samples_opt_m.extend(falsey.samples_opt.clone());
                                samples_opt = samples_opt_m;
                            }

                            let accept_l = self.mgr.and(pred_dist, truthy.accept);
                            let accept_r = self.mgr.and(pred_dist.neg(), falsey.accept);
                            let accept = self.mgr.or(accept_l, accept_r);
                            let accept = self.mgr.and(accept, ctx.accept);

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
                            debug!("importance_true {:?}, importance_false {:?}", importance_true, importance_false);
                            let importance = importance_true + importance_false;
                            debug!("importance {:?}", importance);
                            debug!("=============================");


                            let c = Output {
                                  dists,
                                  accept,
                                  samples,
                                  samples_opt,
                                  weightmap,
                                  substitutions,
                                  probabilities,
                                  importance,
                            };
                            debug_step!("ite", ctx, c);
                            Ok(c)
                        }).collect::<Result<Vec<Output>>>()?;
                        Ok((c, ftr))
                    })
                    .collect::<Result<Vec<(Vec<Output>, ExprTr)>>>()?
                    .into_iter()
                    .fold(
                        (vec![], None),
                        |(mut outs, mbody), (compiled_outs, body)| {
                            outs.extend(compiled_outs);
                            (outs, Some(body))
                        },
                    );
                let ftr = mftr.unwrap();
                let outs: Compiled = c.into_iter().collect();
                Ok((
                    outs.clone(),
                    EIte(Box::new(outs), Box::new(atr), Box::new(ttr), Box::new(ftr)),
                ))
            }
            EFlip(d, param) => {
                let flip = (param * 100.0).round() / 100.0;
                let span = tracing::span!(tracing::Level::DEBUG, "", flip);
                let _enter = span.enter();

                let mut weightmap = ctx.weightmap.clone();
                weightmap.insert(d.var().unsafe_label(), *param);
                let o = Output {
                    dists: vec![self.mgr.var(d.var().unsafe_label(), true)],
                    accept: ctx.accept,
                    samples: ctx.samples,
                    samples_opt: ctx.samples_opt.clone(),
                    weightmap,
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0)],
                    importance: I::Weight(1.0),
                };
                debug_step!("flip", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), EFlip(Box::new(c), *param)))
            }
            EObserve(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "observe");
                let _enter = span.enter();

                let (comp, atr) = self.eval_anf(ctx, a)?;
                debug!("In. Accept {}", &ctx.accept.print_bdd());
                debug!("Comp. dist {}", renderbdds(&comp.dists));
                debug!("weightmap  {:?}", ctx.weightmap);
                let dist = comp
                    .dists
                    .into_iter()
                    .fold(ctx.accept, |global, cur| self.mgr.and(global, cur));

                let var_order = self.order.clone();
                let wmc_params = ctx.weightmap.as_params(self.max_label);
                let avars = crate::utils::variables(dist);
                for (i, var) in avars.iter().enumerate() {
                    debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
                }
                debug!("WMCParams  {:?}", wmc_params);
                debug!("VarOrder   {:?}", var_order);
                debug!("Accept     {}", dist.print_bdd());
                let wmc;
                // FIXME: should be aggregating these stats somewhere
                debug!("using optimizations: {}", self.sample_pruning);
                if self.sample_pruning {
                    (wmc, _) = crate::inference::calculate_wmc_prob(
                        self.mgr,
                        &wmc_params,
                        &var_order,
                        dist,
                        ctx.accept,
                        BddPtr::PtrTrue,
                    );
                } else {
                    (wmc, _) = crate::inference::calculate_wmc_prob(
                        self.mgr,
                        &wmc_params,
                        &var_order,
                        dist,
                        ctx.accept,
                        ctx.samples,
                    );
                }

                let importance = I::Weight(wmc);
                debug!("IWeight    {}", importance.weight());

                let o = Output {
                    dists: vec![BddPtr::PtrTrue],
                    accept: dist,
                    samples: ctx.samples,
                    samples_opt: ctx.samples_opt.clone(),
                    weightmap: ctx.weightmap.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0)],
                    importance,
                };
                debug_step!("observe", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), EObserve(Box::new(c), Box::new(atr))))
            }
            ESample(_, e) => {
                use SamplingContext::*;
                let span = tracing::span!(tracing::Level::DEBUG, "sample");
                let _enter = span.enter();

                match &self.sampling_context {
                    Sampling(_) | Unset => {}
                    Set(v) => {
                        self.sampling_context = Sampling(v.clone());
                    }
                    SamplingWithLet(ctx, lastlet) => {
                        self.sampling_context = Sampling(lastlet.clone());
                    }
                }

                let (comp, etr) = self.eval_expr(ctx, e)?;
                let c: Compiled = comp
                    .into_iter()
                    .enumerate()
                    .map(|(ix, comp)| {
                        let span = if ix == 0 {
                            tracing::span!(tracing::Level::DEBUG, "")
                        } else {
                            tracing::span!(tracing::Level::DEBUG, "", ix)
                        };
                        let _enter = span.enter();

                        let wmc_params = comp.weightmap.as_params(self.max_label);
                        let var_order = self.order.clone();
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
                            let sampling_context = self.sampling_context.clone();
                            let mut samples = comp.samples;
                            let mut samples_opt = comp.samples_opt.clone();
                            let (mut qs, mut dists) = (vec![], vec![]);
                            for dist in comp.dists.iter() {
                                let theta_q;
                                // FIXME: should be aggregating the stats somewhere
                                debug!("using optimizations: {}", self.sample_pruning);
                                if self.sample_pruning {
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
                                let sampled_value = BddPtr::from_bool(sample);
                                dists.push(sampled_value);

                                if !self.sample_pruning {
                                    // sample in sequence. A smarter sample would compile
                                    // all samples of a multi-rooted BDD, but I need to futz
                                    // with rsdd's fold
                                    let dist_holds = self.mgr.iff(*dist, sampled_value);
                                    samples = self.mgr.and(samples, dist_holds);

                                    let dv = sampling_context.as_option();
                                    // TODO technically we have static knowledge of whether or
                                    // not we need to include a sample.
                                    samples_opt.insert(*dist, (dv, sample));
                                }
                            }
                            debug!("final dists:   {}", renderbdds(&dists));
                            debug!("final samples: {}", samples.print_bdd());
                            // println!("sample_pruning: {}", self.sample_pruning);
                            debug!("using optimizations: {}", self.sample_pruning);
                            let c = Output {
                                dists,
                                accept,
                                samples,
                                samples_opt: samples_opt.clone(),
                                weightmap: comp.weightmap.clone(),
                                // any dangling references will be treated as
                                // constant and, thus, ignored -- information
                                // about this will live only in the propagated
                                // weight
                                substitutions: comp.substitutions.clone(),
                                probabilities: qs,
                                importance: I::Weight(1.0),
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

                Ok((c.clone(), ESample(Box::new(c), Box::new(etr))))
            }
        }
    }
}
pub fn debug(env: &mut Env, p: &ProgramAnlys) -> Result<(Compiled, ExprTr)> {
    match p {
        Program::Body(e) => {
            debug!("========================================================");
            env.eval_expr(&Default::default(), e)
        }
    }
}

pub fn compile(env: &mut Env, p: &ProgramAnlys) -> Result<Compiled> {
    Ok(debug(env, p)?.0)
}
