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
use tracing::debug;

pub type Mgr = BddManager<AllTable<BddPtr>>;

pub use crate::compile::compiled::{Compiled, SubstMap};
pub use crate::compile::context::Context;
pub use crate::compile::errors::{CompileError, Result};
pub use crate::compile::importance::{Importance, I};
pub use crate::compile::weighting::{Weight, WeightMap};
use CompileError::*;

pub struct EnvArgs {
    // FIXME: just have env own BddManager and StdRng
    pub names: HashMap<String, UniqueId>,
    pub mgr: BddManager<AllTable<BddPtr>>,
    pub rng: StdRng,
    pub samples: HashMap<UniqueId, Vec<bool>>,
}
impl EnvArgs {
    pub fn default_args(seed: Option<u64>) -> EnvArgs {
        let mgr = BddManager::<AllTable<BddPtr>>::new_default_order(1000);
        let rng = match seed {
            None => StdRng::from_entropy(),
            Some(s) => StdRng::seed_from_u64(s),
        };
        EnvArgs {
            names: HashMap::new(),
            mgr,
            rng,
            samples: HashMap::new(),
        }
    }
}

pub struct Env<'a> {
    pub names: HashMap<String, UniqueId>,
    pub order: Option<VarOrder>,
    pub weightmap: Option<WmcParams<f64>>,
    pub inv: Option<HashMap<VarLabel, Var>>,
    pub varmap: Option<HashMap<UniqueId, Var>>,
    pub max_label: Option<u64>,
    pub mgr: &'a mut BddManager<AllTable<BddPtr>>,
    pub rng: &'a mut StdRng,
    pub samples: HashMap<UniqueId, Vec<bool>>,
}
impl<'a> Env<'a> {
    pub fn new(mgr: &'a mut BddManager<AllTable<BddPtr>>, rng: &'a mut StdRng) -> Env<'a> {
        Env {
            names: HashMap::new(),
            order: None,
            weightmap: None,
            varmap: None,
            max_label: None,
            inv: None,
            mgr,
            rng,
            samples: HashMap::new(),
        }
    }
    pub fn from_args(x: &'a mut EnvArgs) -> Env<'a> {
        Env {
            names: x.names.clone(),
            order: None,
            weightmap: None,
            varmap: None,
            max_label: None,
            inv: None,
            mgr: &mut x.mgr,
            rng: &mut x.rng,
            samples: HashMap::new(),
        }
    }
    pub fn eval_anf_binop(
        &mut self,
        ctx: &Context,
        bl: &AnfAnn,
        br: &AnfAnn,
        op: &dyn Fn(&mut Mgr, BddPtr, BddPtr) -> BddPtr,
    ) -> Result<Compiled> {
        let l = self.eval_anf(ctx, bl)?;
        let r = self.eval_anf(ctx, br)?;
        debug!("[anf_binop][left ] {}", renderbdds(&l.dists));
        debug!("[anf_binop][right] {}", renderbdds(&r.dists));

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
            Ok(Compiled {
                dists,
                accept: ctx.accept.clone(),
                substitutions: ctx.substitutions.clone(),
                weightmap: ctx.weightmap.clone(),
                probabilities: vec![Probability::new(1.0); dists_len],
                importance: I::Weight(1.0),
            })
        }
    }

    pub fn eval_anf(&mut self, ctx: &Context, a: &AnfAnn) -> Result<Compiled> {
        use ANF::*;
        match a {
            AVar(var, s) => match ctx.substitutions.get(&var.id) {
                None => Err(Generic(format!(
                    "variable {} does not reference known substitution",
                    s
                ))),
                Some((subs, subvar)) => Ok(Compiled {
                    dists: subs.to_vec(),
                    accept: ctx.accept.clone(),
                    substitutions: ctx.substitutions.clone(),
                    weightmap: ctx.weightmap.clone(),
                    probabilities: vec![Probability::new(1.0); subs.len()],
                    importance: I::Weight(1.0),
                }),
            },
            AVal(_, Val::Bool(b)) => Ok(Compiled {
                dists: vec![BddPtr::from_bool(*b)],
                accept: ctx.accept.clone(),
                substitutions: ctx.substitutions.clone(),
                weightmap: ctx.weightmap.clone(),
                probabilities: vec![Probability::new(1.0)],
                importance: I::Weight(1.0),
            }),
            AVal(_, Val::Prod(vs)) => Err(CompileError::Todo()),
            And(bl, br) => self.eval_anf_binop(ctx, bl, br, &BddManager::and),
            Or(bl, br) => {
                let x = self.eval_anf_binop(ctx, bl, br, &BddManager::or);
                debug!("[anf_OR][final] {}", renderbdds(&x.as_ref().unwrap().dists));
                x
            }
            Neg(bp) => {
                let mut p = self.eval_anf(ctx, bp)?;
                // FIXME negating a tuple? seems weird!!!!
                p.dists = p.dists.iter().map(BddPtr::neg).collect_vec();

                Ok(p)
            }
        }
    }
    pub fn eval_anf_over(&mut self, ctx: &Context, a: &AnfAnn) -> Result<Vec<Compiled>> {
        let c = self.eval_anf(ctx, a)?;
        Ok(vec![c])
    }

    pub fn eval_expr(&mut self, ctx: &Context, e: &ExprAnn) -> Result<Vec<Compiled>> {
        use Expr::*;
        match e {
            EAnf(_, a) => {
                debug!(">>>anf: {:?}", a);
                let c = self.eval_anf(ctx, a)?;
                debug_compiled!("anf", ctx, &c);
                Ok(vec![c])
            }
            EPrj(_, i, a) => {
                if i > &1 {
                    debug!(">>>prj@{}: {:?}", i, a);
                }
                let mut c = self.eval_anf(ctx, a)?;
                let dists = c.dists;
                c.dists = vec![dists[*i]];
                if i > &1 {
                    debug_compiled!(&format!("prj@{}", i).to_string(), ctx, c);
                }
                Ok(vec![c])
            }
            EFst(_, a) => {
                debug!(">>>fst: {:?}", a);
                let c = self.eval_expr(ctx, &EPrj((), 0, a.clone()))?;
                // debug_compiled!("fst", ctx, c);
                Ok(c)
            }
            ESnd(_, a) => {
                debug!(">>>snd: {:?}", a);
                let c = self.eval_expr(ctx, &EPrj((), 1, a.clone()))?;
                // debug_compiled!("snd", ctx, c);
                Ok(c)
            }
            EProd(_, anfs) => {
                debug!(">>>prod: {:?}", anfs);
                let dists = anfs.iter().fold(Ok(vec![]), |res, a| {
                    let fin = res?;
                    let c = self.eval_anf(ctx, a)?;
                    Ok(fin.iter().chain(&c.dists).cloned().collect_vec())
                })?;
                let flen = dists.len();
                let c = Compiled {
                    dists,
                    accept: ctx.accept.clone(),
                    weightmap: ctx.weightmap.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0); flen],
                    importance: I::Weight(1.0),
                };

                debug_compiled!("prod", ctx, c);
                Ok(vec![c])
            }
            ELetIn(var, s, ebound, ebody) => {
                debug!(">>>let-in {}", s);
                let lbl = var.label;

                // if we produce multiple worlds, we must account for them all
                Ok(self
                    .eval_expr(&ctx, ebound)?
                    .into_iter()
                    .map(|bound| {
                        let mut newctx = Context::from_compiled(&bound);
                        newctx
                            .substitutions
                            .insert(var.id, (bound.dists.clone(), var.clone()));

                        let bodies = self.eval_expr(&newctx, ebody)?;
                        bodies
                            .into_iter()
                            .map(|body| {
                                let accept = self.mgr.and(body.accept, ctx.accept);

                                let probabilities =
                                    izip!(bound.probabilities.clone(), body.probabilities)
                                        .map(|(p1, p2)| p1 * p2)
                                        .collect_vec();
                                let importance =
                                    I::Weight(bound.importance.weight() * body.importance.weight());

                                let c = Compiled {
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
                            .collect::<Result<Vec<Compiled>>>()
                    })
                    .collect::<Result<Vec<Vec<Compiled>>>>()?
                    .into_iter()
                    .flatten()
                    .collect_vec())
            }
            EIte(_, cond, t, f) => {
                let pred = self.eval_anf(ctx, cond)?;
                if !pred.dists.len() == 1 {
                    return Err(TypeError(format!(
                        "Expected Bool for ITE condition\nGot: {cond:?}\n{ctx:?}",
                    )));
                }
                let pred_dist = pred.dists[0];

                Ok(self.eval_expr(ctx, t)?.into_iter().map(|truthy| {
                    self.eval_expr(ctx, f)?.into_iter().map(|falsey| {
if truthy.dists.len() != falsey.dists.len() {
                    return Err(TypeError(format!(
                                          "Expected both branches of ITE to return same len tuple\nGot (left): {:?}\nGot (right):{:?}",
                                          truthy.dists.len(), falsey.dists.len(),
                                      )));
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
                let c = Compiled {
                    dists,
                    accept,
                    weightmap,
                    substitutions,
                    probabilities,
                    importance,
                };
                debug_compiled!("ite", ctx, c);
                        Ok(c)
                    }).collect::<Result<Vec<Compiled>>>()
                }).collect::<Result<Vec<Vec<Compiled>>>>()?
    .into_iter()
                    .flatten()
                    .collect_vec())
            }
            EFlip(var, param) => {
                debug!(">>>flip {:.3}", param);
                let mut weightmap = ctx.weightmap.clone();
                weightmap.insert(var.label.unwrap(), *param);
                let c = Compiled {
                    dists: vec![self.mgr.var(var.label.unwrap(), true)],
                    accept: ctx.accept.clone(),
                    weightmap,
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0)],
                    importance: I::Weight(1.0),
                };
                debug_compiled!("flip", ctx, c);
                Ok(vec![c])
            }
            EObserve(_, a) => {
                debug!(">>>observe");
                let comp = self.eval_anf(ctx, a)?;
                debug!("[observe] In. Accept {}", &ctx.accept.print_bdd());
                debug!("[observe] Comp. dist {}", renderbdds(&comp.dists));
                debug!("[observe] weightmap  {:?}", ctx.weightmap);
                let dist = comp
                    .dists
                    .into_iter()
                    .fold(ctx.accept.clone(), |global, cur| self.mgr.and(global, cur));

                let var_order = self.order.clone().unwrap();
                let wmc_params = ctx.weightmap.as_params(self.max_label.unwrap());
                let avars = crate::utils::variables(dist);
                for (i, var) in avars.iter().enumerate() {
                    debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
                }
                // debug!("[observe] max_var    {}", max_var);
                debug!("[observe] WMCParams  {:?}", wmc_params);
                debug!("[observe] VarOrder   {:?}", var_order);
                debug!("[observe] Accept     {}", dist.print_bdd());
                let (a, z) = crate::inference::calculate_wmc_prob(
                    self.mgr,
                    &wmc_params,
                    &var_order,
                    dist,
                    ctx.accept,
                );
                let importance = I::Weight(a / z);
                debug!("[observe] IWeight    {}", importance.weight());

                let c = Compiled {
                    dists: vec![BddPtr::PtrTrue],
                    accept: dist,
                    weightmap: ctx.weightmap.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0)],
                    importance,
                };
                debug_compiled!("observe", ctx, c);
                Ok(vec![c])
            }
            ESample(_, e) => {
                debug!(">>>sample");
                self.eval_expr(ctx, e)?
                    .into_iter()
                    .map(|comp| {
                        let wmc_params = comp.weightmap.as_params(self.max_label.unwrap());
                        let var_order = self.order.clone().unwrap();
                        debug!("[sample] Incm accept {}", &ctx.accept.print_bdd());
                        debug!("[sample] Comp accept {}", comp.accept.print_bdd());
                        debug!("[sample] Comp distrb {}", renderbdds(&comp.dists));

                        debug!("[sample] weight_map {:?}", &comp.weightmap);
                        debug!("[sample] WMCParams  {:?}", wmc_params);
                        debug!("[sample] VarOrder   {:?}", var_order);

                        let (accept, qs, dists): (BddPtr, Vec<Probability>, Vec<BddPtr>) =
                            comp.dists.iter().fold(
                                (comp.accept, vec![], vec![]),
                                |(accept, mut qs, mut dists), dist| {
                                    let sample_dist = self.mgr.and(accept, *dist);
                                    let (a, z) = crate::inference::calculate_wmc_prob(
                                        self.mgr,
                                        &wmc_params,
                                        &var_order,
                                        sample_dist,
                                        accept,
                                    );
                                    let theta_q = a / z;

                                    let bern = Bernoulli::new(theta_q).unwrap();
                                    let sample = bern.sample(self.rng);
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
                                    let new_accept = self.mgr.and(accept, dist_holds);
                                    (new_accept, qs, dists)
                                },
                            );
                        debug!("[sample] final samples: {}", renderbdds(&dists));
                        debug!("[sample] final accept : {}", accept.print_bdd());

                        let c = Compiled {
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
                        Ok(c)
                    })
                    .collect()
            }
        }
    }
}
// pub fn compile(env: &mut Env, p: &ProgramAnn) -> Result<Vec<Compiled>, CompileError> {
//     match p {
//         Program::Body(e) => {
//             debug!("========================================================");
//             env.eval_expr(&Default::default(), e)
//         }
//     }
// }

// use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
// use tracing::*;

// pub fn debug(p: &ProgramTyped) -> Result<Vec<Compiled>, CompileError> {
//     use crate::annotate::LabelEnv;
//     use crate::typecheck::typecheck;
//     use crate::uniquify::SymEnv;

//     let p = typecheck(p)?;
//     let mut senv = SymEnv::default();
//     let p = senv.uniquify(&p)?;
//     let mut lenv = LabelEnv::new();
//     let (p, vo, varmap, inv, mxlbl) = lenv.annotate(&p)?;

//     let mut env_args = EnvArgs::default_args(None);
//     let mut env = Env::from_args(&mut env_args);
//     env.names = senv.names; // just for debugging, really.
//     env.order = Some(vo);
//     env.max_label = Some(mxlbl);
//     env.varmap = Some(varmap);
//     env.inv = Some(inv);
//     let c = compile(&mut env, &p);
//     tracing::debug!("hurray!");
//     c
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::compile::*;
//     use crate::grammar::*;
//     use crate::grammar_macros::*;
//     use rsdd::builder::bdd_builder::*;
//     use rsdd::builder::cache::all_app::*;

//     use rsdd::repr::bdd::*;
//     use rsdd::repr::ddnnf::DDNNFPtr;
//     use rsdd::repr::var_label::*;
//     use rsdd::*;
//     use tracing_test::traced_test;

//     #[test]
//     #[traced_test]
//     fn enumerate_samples() {
//         let mk = |ret: ExprTyped| {
//             Program::Body(lets![
//                 "x" ; b!() ;= flip!(1/5);
//                 // "y" ; b!() ;= ite!(
//                 //     if ( var!("x") )
//                 //     then { sample!(flip!(1/3)) }
//                 //     else { flip!(1/4) });
//                 ...? ret ; b!()
//             ])
//         };
//         let p = mk(b!("y"));

//         match debug(&p) {
//             Ok(cs) => {
//                 debug!("{:?}", cs);
//             }
//             Err(e) => panic!("{:?}", e),
//         }
//     }
// }
