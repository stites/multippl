pub mod compiled;
pub mod context;
pub mod errors;
pub mod importance;
pub mod weighting;

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
use rsdd::repr::wmc::WmcParams;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;
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

    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    pub struct Var {
        pub id: UniqueId,               // associated unique ids
        pub label: Option<VarLabel>,    // only hold values in the final formula
        pub provenance: Option<String>, // when None, this indicates that the variable is in the final formula
    }
    impl Var {
        pub fn new(id: UniqueId, label: Option<VarLabel>, provenance: Option<String>) -> Self {
            Self {
                id,
                label,
                provenance,
            }
        }
    }

    impl fmt::Display for Var {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "Var({:?}->{:?}: {:?}, ",
                self.id, self.label, self.provenance
            )
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Trace;

    impl ξ<Trace> for AVarExt {
        type Ext = Box<Output>;
    }
    impl ξ<Trace> for AValExt {
        type Ext = Box<Output>;
    }
    pub type AnfTr = ANF<Trace>;

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
pub struct Env<'a> {
    pub mgr: &'a mut BddManager<AllTable<BddPtr>>,
    pub rng: Option<&'a mut StdRng>, // None implies "debug mode"

    // pre-computed
    pub order: VarOrder,
    pub max_label: u64,

    // ignored
    pub weightmap: Option<WmcParams<f64>>,
    pub inv: Option<HashMap<VarLabel, Var>>,
    pub varmap: Option<HashMap<UniqueId, Var>>,
    pub samples: HashMap<UniqueId, Vec<bool>>,
}
impl<'a> Env<'a> {
    pub fn new(mgr: &'a mut BddManager<AllTable<BddPtr>>, rng: Option<&'a mut StdRng>) -> Env<'a> {
        Env {
            order: mgr.get_order().clone(),
            weightmap: None,
            varmap: None,
            max_label: mgr.get_order().num_vars() as u64,
            inv: None,
            mgr,
            rng,
            samples: HashMap::new(),
        }
    }

    pub fn eval_anf_binop(
        &mut self,
        ctx: &Context,
        bl: &AnfAnn,
        br: &AnfAnn,
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

    pub fn eval_anf(&mut self, ctx: &Context, a: &AnfAnn) -> Result<(Output, AnfTr)> {
        use ANF::*;
        match a {
            AVar(var, s) => match ctx.substitutions.get(&var.id) {
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
                Ok((p.clone(), Neg(Box::new(ptr))))
            }
        }
    }
    pub fn result_from(&self, c: Output) -> Result<Compiled> {
        Ok(match self.rng {
            Some(_) => Compiled::from_output(c),
            None => Compiled::Output(c),
        })
    }
    pub fn eval_expr(&mut self, ctx: &Context, e: &ExprAnn) -> Result<(Compiled, ExprTr)> {
        use Expr::*;
        match e {
            EAnf(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "anf");
                let _enter = span.enter();
                debug!("{:?}", a);
                let (o, atr) = self.eval_anf(ctx, a)?;
                debug_compiled!("anf", ctx, &o);
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
                    debug_compiled!(&format!("prj@{}", i).to_string(), ctx, o);
                }
                let c = Compiled::Output(o);
                Ok((c.clone(), EAnf(Box::new(c), Box::new(atr))))
            }
            EFst(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "fst");
                let _enter = span.enter();
                debug!("{:?}", a);
                let c = self.eval_expr(ctx, &EPrj((), 0, a.clone()))?;
                // debug_compiled!("fst", ctx, c);
                Ok(c.clone())
            }
            ESnd(_, a) => {
                let span = tracing::span!(tracing::Level::DEBUG, "snd");
                let _enter = span.enter();
                debug!("{:?}", a);
                let c = self.eval_expr(ctx, &EPrj((), 1, a.clone()))?;
                // debug_compiled!("snd", ctx, c);
                Ok(c.clone())
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
                    accept: ctx.accept.clone(),
                    weightmap: ctx.weightmap.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0); flen],
                    importance: I::Weight(1.0),
                };

                debug_compiled!("prod", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), EProd(Box::new(c), atrs)))
            }
            ELetIn(var, s, ebound, ebody) => {
                let let_in_span = tracing::span!(Level::DEBUG, "let", var = s);
                let _enter = let_in_span.enter();

                let lbl = var.label;
                // if we produce multiple worlds, we must account for them all
                let (cbound, eboundtr) = self.eval_expr(&ctx, ebound)?;
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
                            .insert(var.id, (bound.dists.clone(), var.clone()));

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

                                let probabilities =
                                    izip!(bound.probabilities.clone(), body.probabilities)
                                        .map(|(p1, p2)| p1 * p2)
                                        .collect_vec();
                                let importance =
                                    I::Weight(bound.importance.weight() * body.importance.weight());

                                let c = Output {
                                    dists: body.dists,
                                    accept,
                                    substitutions: body.substitutions.clone(),
                                    weightmap: body.weightmap.clone(),
                                    probabilities,
                                    importance,
                                };
                                debug_compiled!(format!("let-in {}", s), ctx, c);
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

                            let accept_l = self.mgr.and(pred_dist, truthy.accept);
                            let accept_r = self.mgr.and(pred_dist.neg(), falsey.accept);
                            let accept = self.mgr.or(accept_l, accept_r);
                            let accept = self.mgr.and(accept, ctx.accept);

                            let mut substitutions = truthy.substitutions.clone();
                            substitutions.extend(falsey.substitutions.clone());
                            let mut weightmap = truthy.weightmap.clone();
                            weightmap.weights.extend(falsey.weightmap.clone());

                            let probabilities = izip!(&truthy.probabilities, &falsey.probabilities)
                                  // dancing with the numerically unstable devil
                                  .map(|(t, f)| (*t * Probability::new(0.5) + *f * Probability::new(0.5)))
                                  .collect_vec();
                            let importance = truthy.convex_combination(&falsey);
                            let c = Output {
                                  dists,
                                  accept,
                                  weightmap,
                                  substitutions,
                                  probabilities,
                                  importance,
                            };
                            debug_compiled!("ite", ctx, c);
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
            EFlip(var, param) => {
                let flip = (param * 100.0).round() / 100.0;
                let span = tracing::span!(tracing::Level::DEBUG, "", flip);
                let _enter = span.enter();

                let mut weightmap = ctx.weightmap.clone();
                weightmap.insert(var.label.unwrap(), *param);
                let o = Output {
                    dists: vec![self.mgr.var(var.label.unwrap(), true)],
                    accept: ctx.accept.clone(),
                    weightmap,
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0)],
                    importance: I::Weight(1.0),
                };
                debug_compiled!("flip", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), EFlip(Box::new(c), param.clone())))
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
                    .fold(ctx.accept.clone(), |global, cur| self.mgr.and(global, cur));

                let var_order = self.order.clone();
                let wmc_params = ctx.weightmap.as_params(self.max_label);
                let avars = crate::utils::variables(dist);
                for (i, var) in avars.iter().enumerate() {
                    debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
                }
                // debug!("[observe] max_var    {}", max_var);
                debug!("WMCParams  {:?}", wmc_params);
                debug!("VarOrder   {:?}", var_order);
                debug!("Accept     {}", dist.print_bdd());
                let (a, z) = crate::inference::calculate_wmc_prob(
                    self.mgr,
                    &wmc_params,
                    &var_order,
                    dist,
                    ctx.accept,
                );
                let importance = I::Weight(a / z);
                debug!("IWeight    {}", importance.weight());

                let o = Output {
                    dists: vec![BddPtr::PtrTrue],
                    accept: dist,
                    weightmap: ctx.weightmap.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0)],
                    importance,
                };
                debug_compiled!("observe", ctx, o);
                let c = Compiled::Output(o);
                Ok((c.clone(), EObserve(Box::new(c), Box::new(atr))))
            }
            ESample(_, e) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample");
                let _enter = span.enter();

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

                        let mut fin = vec![];

                        for sample_det in [true, false] {
                            let span = if sample_det {
                                tracing::span!(tracing::Level::DEBUG, "")
                            } else {
                                let s = false;
                                tracing::span!(tracing::Level::DEBUG, "", s)
                            };
                            let _enter = span.enter();

                            let (mut accept, mut qs, mut dists) = (comp.accept, vec![], vec![]);
                            for dist in comp.dists.iter() {
                                let sample_dist = self.mgr.and(accept, *dist);
                                let (a, z) = crate::inference::calculate_wmc_prob(
                                    self.mgr,
                                    &wmc_params,
                                    &var_order,
                                    sample_dist,
                                    accept,
                                );
                                let theta_q = a / z;

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

                                // sample in sequence. A smarter sample would compile
                                // all samples of a multi-rooted BDD, but I need to futz
                                // with rsdd's fold
                                let dist_holds = self.mgr.iff(*dist, sampled_value);
                                accept = self.mgr.and(accept, dist_holds);
                            }
                            debug!("final samples: {}", renderbdds(&dists));
                            debug!("final accept : {}", accept.print_bdd());

                            let c = Output {
                                dists,
                                accept,
                                // weightmap: ctx.weightmap.clone(), // FIXME
                                // substitutions: ctx.substitutions.clone(), // TODO any dangling references will be treated as constant and, thus, ignored -- information about this will live only in the propagated weight
                                weightmap: comp.weightmap.clone(), // FIXME
                                substitutions: comp.substitutions.clone(), // TODO any dangling references will be treated as constant and, thus, ignored -- information about this will live only in the propagated weight
                                probabilities: qs,
                                importance: I::Weight(1.0),
                            };
                            debug_compiled!("sample", ctx, c);
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
pub fn debug(env: &mut Env, p: &ProgramAnn) -> Result<(Compiled, ExprTr)> {
    match p {
        Program::Body(e) => {
            debug!("========================================================");
            env.eval_expr(&Default::default(), e)
        }
    }
}

pub fn compile(env: &mut Env, p: &ProgramAnn) -> Result<Compiled> {
    Ok(debug(env, p)?.0)
}
