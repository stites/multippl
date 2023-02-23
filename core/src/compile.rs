use crate::annotate::grammar::*;
use crate::grammar::*;
use crate::render::*;
use crate::uniquify::grammar::UniqueId;
use itertools::*;
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

type Mgr = BddManager<AllTable<BddPtr>>;

fn leaf_variable(bdd: BddPtr) -> Option<VarLabel> {
    let n = bdd.into_node_safe()?;
    if (n.low == BddPtr::PtrTrue && n.high == BddPtr::PtrFalse)
        || (n.low == BddPtr::PtrFalse && n.high == BddPtr::PtrTrue)
    {
        Some(n.var)
    } else {
        None
    }
}

fn variables(bdd: BddPtr, order: VarOrder) -> Vec<VarLabel> {
    Fold::new(
        &mut |vs: Vec<Option<VarLabel>>, bdd| {
            let mut vs = vs.clone();
            vs.push(bdd.node.var_safe());
            vs
        },
        vec![],
        &|ret, lo_hi| match lo_hi {
            None => ret,
            Some((lo, hi)) => {
                let mut v = ret.clone();
                v.extend(lo);
                v.extend(hi);
                v
            }
        },
    )
    .mut_fold(&bdd)
    .into_iter()
    .filter_map(|x| x)
    .collect()
}

fn debug_compiled(s: &str, ctx: &Context, c: &Compiled) {
    let w = &ctx.weight_map;
    let p = &ctx.substitutions;
    let renderw = |ws: &WeightMap| {
        ws.iter()
            .map(|(k, (l, h))| format!("{k}: ({l}, {h})"))
            .join(", ")
    };
    let renderp = |ps: &SubstMap| {
        ps.iter()
            .map(|(k, (v, _))| format!("{k}: {}", renderbdds(v)))
            .join(", ")
    };

    let dists = renderbdds(&c.dists);

    let accepts = format!("{}", c.accept.print_bdd());

    debug!("{s}, [{}], [{}]", renderw(w), renderp(p));
    debug!("      \\||/  {}", dists);
    debug!("      \\||/  {}", accepts);
    debug!("      \\||/  {}", renderw(&c.weight_map));
    debug!("      \\||/  {}", renderp(&c.substitutions));
    debug!("      \\||/  {}", fmt_f64(false)(c.importance_weight));
    debug!("----------------------------------------");
}

#[derive(Clone, Eq, Hash, PartialEq, Debug)]
pub enum CompileError {
    AcceptingNonZeroError(String),
    Todo(),
    TypeError(String),
    Generic(String),
    SemanticsError(String),
}
impl CompileError {
    pub fn to_string(&self) -> String {
        match self {
            AcceptingNonZeroError(s) => s.to_string(),
            Todo() => "todo!".to_string(),
            TypeError(s) => s.to_string(),
            Generic(s) => s.to_string(),
            SemanticsError(s) => s.to_string(),
        }
    }
}
use CompileError::*;

pub type WeightMap = HashMap<UniqueId, (f64, f64)>;
pub type SubstMap = HashMap<UniqueId, (Vec<BddPtr>, Var)>;

fn const_weight() -> (f64, f64) {
    (0.5, 0.5)
}
pub fn weight_map_to_params(m: &WeightMap) -> (WmcParams<f64>, u64) {
    let mut wmc_params = WmcParams::new(0.0, 1.0);
    let mut max = 0;
    for (lbl, (l, h)) in m {
        wmc_params.set_weight(VarLabel::new(lbl.0), *l, *h);
        if lbl.0 > max {
            max = lbl.0;
        }
    }
    debug!(max = max);
    (wmc_params, max)
}

#[derive(Debug, Clone)]
pub struct Context {
    pub gamma: Γ,
    pub accept: BddPtr,
    pub weight_map: WeightMap,
    pub substitutions: SubstMap,
}
impl Default for Context {
    fn default() -> Self {
        Context {
            gamma: Γ(Default::default()),
            accept: BddPtr::PtrTrue,
            weight_map: Default::default(),
            substitutions: Default::default(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Compiled {
    pub dists: Vec<BddPtr>,
    pub accept: BddPtr,
    pub weight_map: WeightMap, // must be a hashmap as sample will collapse variables
    pub probabilities: Vec<Probability>,
    pub substitutions: SubstMap,
    pub importance_weight: f64,
}
impl Compiled {
    fn convex_combination(&self, o: &Compiled) -> f64 {
        izip!(&self.probabilities, &o.probabilities,).fold(0.0, |res, (selfp, op)| {
            (selfp.as_f64() * self.importance_weight + op.as_f64() * o.importance_weight) / 2.0
        })
    }
    fn default(dist: BddPtr) -> Compiled {
        Compiled::default_vec(vec![dist])
    }
    fn default_vec(dists: Vec<BddPtr>) -> Compiled {
        let probabilities = vec![Probability::new(1.0); dists.len()];
        Compiled {
            dists,
            probabilities,
            accept: BddPtr::PtrTrue,
            weight_map: HashMap::new(),
            substitutions: HashMap::new(),
            importance_weight: 1.0,
        }
    }
}

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
    ) -> Result<Compiled, CompileError> {
        let l = self.eval_anf(ctx, bl)?;
        let r = self.eval_anf(ctx, br)?;
        let mut weight_map = l.weight_map.clone();
        weight_map.extend(r.weight_map.clone());
        let mut substitutions = l.substitutions.clone();
        substitutions.extend(r.substitutions.clone());
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
            Ok(Compiled {
                accept: ctx.accept.clone(),
                weight_map: ctx.weight_map.clone(),
                substitutions: ctx.substitutions.clone(),
                ..Compiled::default_vec(dists)
            })
        }
    }
    pub fn eval_anf(&mut self, ctx: &Context, a: &AnfAnn) -> Result<Compiled, CompileError> {
        use ANF::*;
        match a {
            AVar(var, s) => match ctx.substitutions.get(&var.id) {
                None => Err(Generic(
                    "variable does not reference known substitution".to_string(),
                )),
                Some((subs, subvar)) => Ok(Compiled {
                    accept: ctx.accept.clone(),
                    substitutions: ctx.substitutions.clone(),
                    weight_map: ctx.weight_map.clone(),
                    ..Compiled::default_vec(subs.to_vec())
                }),
            },
            AVal(_, Val::Bool(b)) => Ok(Compiled {
                accept: ctx.accept.clone(),
                substitutions: ctx.substitutions.clone(),
                weight_map: ctx.weight_map.clone(),
                ..Compiled::default(BddPtr::from_bool(*b))
            }),
            AVal(_, Val::Prod(vs)) => Err(CompileError::Todo()),
            And(bl, br) => self.eval_anf_binop(ctx, bl, br, &BddManager::and),
            Or(bl, br) => self.eval_anf_binop(ctx, bl, br, &BddManager::or),
            Neg(bp) => {
                let mut p = self.eval_anf(ctx, bp)?;
                // FIXME negating a tuple? seems weird!!!!
                p.dists = p.dists.iter().map(BddPtr::neg).collect_vec();

                Ok(p)
            }
        }
    }

    pub fn apply_substitutions1(&mut self, bdd: BddPtr, p: &SubstMap) -> Vec<BddPtr> {
        println!("substitutions: {}", rendersubs(p));
        match leaf_variable(bdd) {
            Some(lbl) => match p.get(&UniqueId::from_lbl(lbl)) {
                None => {
                    debug!("isleaf, no substitution found for bdd: {}", bdd.print_bdd());
                    vec![bdd]
                }
                Some((subs, _)) => {
                    debug!(
                        "isleaf, swapping {lbl:?}: {} -> {}",
                        bdd.print_bdd(),
                        renderbdds(subs)
                    );
                    subs.clone()
                }
            },
            None => {
                let mut cur = bdd.clone();
                // doing this stupid looping to make sure substitutions are fully normalized.
                // this is a problem, for instance, when you have ITE and observe statements interacting.
                loop {
                    let finl = p.iter().fold(cur, |fin, (lbl, (sub, v))| {
                        if sub.len() > 1 {
                            fin // subs are intended to be a product and should be replacing a variable.
                        } else {
                            let x = fin.clone();
                            let c = self.mgr.compose(fin, lbl.as_lbl(), sub[0]);
                            if x != c {
                                debug!(
                                    "[substituting] notleaf: apply sub: {:?}@{}",
                                    v.id,
                                    sub[0].print_bdd(),
                                );
                                debug!(
                                    "[substituting] notleaf: transform: {} => {}",
                                    x.print_bdd(),
                                    c.print_bdd()
                                );
                            }
                            c
                        }
                    });
                    if cur == finl {
                        if bdd.clone() == cur {
                            debug!(
                                "[substituting] no substitution found for {}",
                                bdd.print_bdd()
                            );
                        } else {
                            debug!(
                                "[substituting] final: {} -> {}",
                                bdd.print_bdd(),
                                finl.print_bdd()
                            );
                        }
                        return vec![finl];
                    } else {
                        cur = finl.clone();
                    }
                }
            }
        }
    }

    pub fn apply_substitutions(&mut self, bdds: Vec<BddPtr>, p: &SubstMap) -> Vec<BddPtr> {
        // punt on typed substitution, just assume a nice user
        match bdds.first() {
            None => vec![],
            Some(hd) => {
                let mut hds = self.apply_substitutions1(*hd, p);
                let tl = &bdds[1..];
                let tl = self.apply_substitutions(tl.to_vec(), p);
                hds.extend(tl);
                hds
            }
        }
    }

    pub fn log_samples(&mut self, id: UniqueId, ebound: &ExprAnn, bound: &Compiled) {
        if ebound.is_sample() {
            let samples = bound
                .dists
                .iter()
                .map(|dist| match dist {
                    BddPtr::PtrTrue => true,
                    BddPtr::PtrFalse => false,
                    _ => panic!("impossible"),
                })
                .collect_vec();
            self.samples.insert(id, samples);
        }
    }

    pub fn eval_expr(&mut self, ctx: &Context, e: &ExprAnn) -> Result<Compiled, CompileError> {
        use Expr::*;
        match e {
            EAnf(_, a) => {
                debug!(">>>anf: {:?}", a);
                let c = self.eval_anf(ctx, a)?;
                debug_compiled("anf", ctx, &c);
                Ok(c)
            }
            EPrj(_, i, a) => {
                if i > &1 {
                    debug!(">>>prj@{}: {:?}", i, a);
                }
                // ignore types for now.
                // let aty = a.as_type();
                // assert!(aty.left() == Some(*ty.clone()), "actual {:?} != expected {:?}. type is: {:?}", aty.left(), Some(*ty.clone()), ty);
                let mut c = self.eval_anf(ctx, a)?;
                // let dists = self.apply_substitutions(c.dists, &ctx.substitutions);
                let dists = c.dists;
                c.dists = vec![dists[*i]];
                if i > &1 {
                    debug_compiled(&format!("prj@{}", i).to_string(), ctx, &c);
                }
                Ok(c)
            }
            EFst(_, a) => {
                debug!(">>>fst: {:?}", a);
                let c = self.eval_expr(ctx, &EPrj((), 0, a.clone()))?;
                debug_compiled("fst", ctx, &c);
                Ok(c)
            }
            ESnd(_, a) => {
                debug!(">>>snd: {:?}", a);
                let c = self.eval_expr(ctx, &EPrj((), 1, a.clone()))?;
                debug_compiled("snd", ctx, &c);
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
                    weight_map: ctx.weight_map.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0); flen],
                    importance_weight: 1.0,
                };

                debug_compiled("prod", ctx, &c);
                Ok(c)
            }
            ELetIn(var, s, ebound, ebody) => {
                debug!(">>>let-in {}", s);
                let lbl = var.label;
                let mut wm = ctx.weight_map.clone();
                wm.insert(var.id, const_weight());

                let mut newctx = ctx.clone();
                newctx.weight_map = wm;

                let bound = self.eval_expr(&newctx, ebound)?;
                let mut bound_substitutions = bound.substitutions.clone();
                bound_substitutions.insert(var.id, (bound.dists.clone(), var.clone()));

                let mut newctx = newctx.clone();
                newctx.weight_map = bound.weight_map.clone();
                newctx.substitutions = bound_substitutions.clone();

                let body = self.eval_expr(&newctx, ebody)?;
                let mut weight_map = bound.weight_map.clone();
                weight_map.extend(body.weight_map);

                let mut substitutions = body.substitutions.clone();
                substitutions.extend(bound_substitutions); // FIXME: almost certainly redundant
                let dists = body.dists; // .apply_substitutions(body.dists, &substitutions);
                let accept = self.mgr.and(bound.accept, body.accept);
                let accept = self.mgr.and(accept, ctx.accept);

                self.log_samples(var.id, ebound, &bound);

                let probabilities = izip!(bound.probabilities, body.probabilities)
                    .map(|(p1, p2)| p1 * p2)
                    .collect_vec();
                let importance_weight = bound.importance_weight * body.importance_weight;

                let c = Compiled {
                    dists,
                    accept,
                    weight_map,
                    substitutions,
                    probabilities,
                    importance_weight,
                };
                debug_compiled(&format!("let-in {}", s), ctx, &c);
                Ok(c)
            }
            EIte(_, cond, t, f) => {
                let pred = self.eval_anf(ctx, cond)?;
                if !pred.dists.len() == 1 {
                    return Err(TypeError(format!(
                        "Expected Bool for ITE condition\nGot: {cond:?}\n{ctx:?}",
                    )));
                }
                let pred_dist = pred.dists[0];

                let truthy = self.eval_expr(ctx, t)?;
                let falsey = self.eval_expr(ctx, f)?;
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

                let mut weight_map = truthy.weight_map.clone();
                weight_map.extend(falsey.weight_map.clone());
                let mut substitutions = truthy.substitutions.clone();
                substitutions.extend(falsey.substitutions.clone());

                let probabilities = izip!(&truthy.probabilities, &falsey.probabilities)
                    // dancing with the numerically unstable devil
                    .map(|(t, f)| (*t * Probability::new(0.5) + *f * Probability::new(0.5)))
                    .collect_vec();
                let importance_weight = truthy.convex_combination(&falsey);
                let c = Compiled {
                    dists,
                    accept,
                    weight_map,
                    substitutions,
                    probabilities,
                    importance_weight,
                };
                debug_compiled("ite", ctx, &c);
                Ok(c)
            }
            EFlip(var, param) => {
                debug!(">>>flip {}", param);
                // let sym = self.fresh();
                // assert_eq!(*vs, sym);
                let mut weight_map = ctx.weight_map.clone();
                let lbl = var.label;
                weight_map.insert(var.id, (1.0 - *param, *param));

                let c = Compiled {
                    dists: vec![self.mgr.var(lbl, true)],
                    accept: ctx.accept.clone(),
                    weight_map,
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0)],
                    importance_weight: 1.0,
                };
                debug_compiled("flip {param}", ctx, &c);
                Ok(c)
            }
            EObserve(_, a) => {
                debug!(">>>observe");
                let comp = self.eval_anf(ctx, a)?;
                debug!("[observe] In. Accept {}", &ctx.accept.print_bdd());
                debug!("[observe] Comp. dist {}", renderbdds(&comp.dists));
                let accept = // self
                    // .apply_substitutions(comp.dists, &ctx.substitutions)
                    comp.dists
                    .into_iter()
                    .fold(ctx.accept.clone(), |global, cur| self.mgr.and(global, cur));

                // let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                // let var_order = VarOrder::linear_order((max_var + 1) as usize);
                let wmc_params = self.weightmap.clone().unwrap();
                let var_order = self.order.clone().unwrap();
                debug!("[observe] weight_map {:?}", &comp.weight_map);
                // debug!("[observe] max_var    {}", max_var);
                debug!("[observe] WMCParams  {:?}", wmc_params);
                debug!("[observe] VarOrder   {:?}", var_order);
                debug!("[observe] Accept     {}", accept.print_bdd());
                let importance_weight = accept.wmc(&var_order, &wmc_params);
                debug!("[observe] IWeight    {}", importance_weight);

                let c = Compiled {
                    dists: vec![BddPtr::PtrTrue],
                    accept,
                    weight_map: ctx.weight_map.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: vec![Probability::new(1.0)],
                    importance_weight,
                };
                debug_compiled("observe", ctx, &c);
                Ok(c)
            }
            ESample(_, e) => {
                debug!(">>>sample");
                let comp = self.eval_expr(ctx, e)?;

                // let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                // let var_order = VarOrder::linear_order((max_var + 1) as usize);
                let wmc_params = self.weightmap.clone().unwrap();
                let var_order = self.order.clone().unwrap();
                debug!("[sample] weight_map {:?}", &comp.weight_map);
                // debug!("[sample] max_var    {}", max_var);
                debug!("[sample] WMCParams  {:?}", wmc_params);
                debug!("[sample] VarOrder   {:?}", var_order);
                // let comp_dists = self.apply_substitutions(comp.dists, &ctx.substitutions);
                let comp_dists = comp.dists;

                let (qs, dists): (Vec<Probability>, Vec<BddPtr>) = comp_dists
                    .iter()
                    .map(|dist| {
                        // FIXME: okay to unify accepts, should I conjoin them here???
                        let theta_q = dist.wmc(&var_order, &wmc_params) as f64;
                        let bern = Bernoulli::new(theta_q).unwrap();
                        let sample = bern.sample(self.rng);
                        let q = Probability::new(if sample { theta_q } else { 1.0 - theta_q });
                        let dists = BddPtr::from_bool(sample);
                        (q, dists)
                    })
                    .unzip();

                debug!(dists = renderbdds(&dists));
                let accept =
                    izip!(comp_dists, &dists).fold(comp.accept.clone(), |global, (dist, v)| {
                        let dist_holds = self.mgr.iff(dist, *v);
                        self.mgr.and(global, dist_holds)
                    });
                let accept = self.mgr.and(accept, ctx.accept);
                debug!(accept = accept.print_bdd());

                let c = Compiled {
                    dists,
                    accept,
                    weight_map: comp.weight_map.clone(),
                    substitutions: ctx.substitutions.clone(),
                    probabilities: qs,
                    importance_weight: 1.0,
                };
                debug_compiled("sample", ctx, &c);
                Ok(c)
            }
        }
    }
}
pub fn compile(env: &mut Env, p: &ProgramAnn) -> Result<Compiled, CompileError> {
    match p {
        Program::Body(e) => {
            debug!("========================================================");
            env.eval_expr(&Default::default(), e)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tracing_test::*;

    pub fn apply_substitutions1(mgr: &mut Mgr, bdd: BddPtr, p: &SubstMap) -> Vec<BddPtr> {
        // println!("substitutions: {}", rendersubs(p));
        match leaf_variable(bdd) {
            Some(lbl) => match p.get(&UniqueId::from_lbl(lbl)) {
                None => {
                    debug!("isleaf, no substitution found for bdd: {}", bdd.print_bdd());
                    vec![bdd]
                }
                Some((subs, _)) => {
                    debug!(
                        "isleaf, swapping {lbl:?}: {} -> {}",
                        bdd.print_bdd(),
                        renderbdds(subs)
                    );
                    subs.clone()
                }
            },
            None => {
                let mut cur = bdd.clone();
                // doing this stupid looping to make sure substitutions are fully normalized.
                // this is a problem, for instance, when you have ITE and observe statements interacting.
                loop {
                    let finl = p.iter().fold(cur, |fin, (lbl, (sub, v))| {
                        if sub.len() > 1 {
                            fin // subs are intended to be a product and should be replacing a variable.
                        } else {
                            let x = fin.clone();
                            let c = mgr.compose(fin, lbl.as_lbl(), sub[0]);
                            if x != c {
                                debug!(
                                    "[substituting] notleaf: apply sub: {:?}@{}",
                                    v.id,
                                    sub[0].print_bdd(),
                                );
                                debug!(
                                    "[substituting] notleaf: transform: {} => {}",
                                    x.print_bdd(),
                                    c.print_bdd()
                                );
                            }
                            c
                        }
                    });
                    if cur == finl {
                        if bdd.clone() == cur {
                            debug!(
                                "[substituting] no substitution found for {}",
                                bdd.print_bdd()
                            );
                        } else {
                            debug!(
                                "[substituting] final: {} -> {}",
                                bdd.print_bdd(),
                                finl.print_bdd()
                            );
                        }
                        return vec![finl];
                    } else {
                        cur = finl.clone();
                    }
                }
            }
        }
    }

    pub fn apply_substitutions(mgr: &mut Mgr, bdds: Vec<BddPtr>, p: &SubstMap) -> Vec<BddPtr> {
        // punt on typed substitution, just assume a nice user
        match bdds.first() {
            None => vec![],
            Some(hd) => {
                let mut hds = apply_substitutions1(mgr, *hd, p);
                let tl = &bdds[1..];
                let tl = apply_substitutions(mgr, tl.to_vec(), p);
                hds.extend(tl);
                hds
            }
        }
    }

    #[test]
    #[ignore]
    #[traced_test]
    fn test_bdd_manager() {
        let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(6);
        let lxv = VarLabel::new(0);
        let lxp = VarLabel::new(1);
        let x_var = mgr.var(lxv, true);
        let x_ptr = mgr.var(lxp, true);

        let lyv = VarLabel::new(2);
        let y_var = mgr.var(lyv, true);

        let lx0v = VarLabel::new(3);
        let lx0p = VarLabel::new(4);
        let x0_var = mgr.var(lx0v, true);
        let x0_ptr = mgr.var(lx0p, true);

        let y_ptr = mgr.or(x0_var, x_var);

        let y_expected = mgr.or(x0_ptr, x_ptr);

        let mut substitutions: SubstMap = HashMap::new();
        let mut add_sub = |lbl, ptr| {
            substitutions.insert(
                UniqueId::from_lbl(lbl),
                (
                    vec![ptr],
                    Var::new(UniqueId::from_lbl(lbl), lbl, true, None),
                ),
            )
        };
        add_sub(lx0v, x0_ptr); // 3 -> 4
        add_sub(lxv, x_ptr); // 0 -> 1
        add_sub(lyv, y_expected); // 2 -> (1, (4, F, T), T) y_ptr

        // first bug I am seeing: (0, (2, F, T), T) -> (4, F, T)
        // return of x || y collapses to x0
        //
        // substitutions:
        // - UniqueId(5)@(#5L5Ctrue): [T]
        // - UniqueId(2)@(#2L2Ctrue): [(1, (4, F, T), T)]
        // - UniqueId(3)@(#3L3Ctrue): [(4, F, T)]
        // - UniqueId(0)@(#0L0Ctrue): [(1, F, T)]
        //
        // [substituting] notleaf: apply sub: UniqueId(2)@(1, (4, F, T), T)
        // [substituting] notleaf: transform: (0, (2, F, T), T) => (1, (4, F, T), T)
        // [substituting] notleaf: apply sub: UniqueId(2)@(1, (4, F, T), T)
        // [substituting] notleaf: transform: (1, (4, F, T), T) => (4, F, T)
        // [substituting] final: (0, (2, F, T), T) -> (4, F, T)
        let observation = mgr.or(x_var, y_var);
        let bug = apply_substitutions(&mut mgr, vec![observation], &substitutions);
        assert_eq!(bug.len(), 1);
        assert_eq!(
            bug[0],
            y_expected,
            "{} != {}",
            bug[0].print_bdd(),
            y_expected.print_bdd()
        );

        let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(6);
        // x variable point to the x bdd ptr
        let x_var = mgr.var(VarLabel::new(0), true); // (0, F, T)
        let x_ptr = mgr.var(VarLabel::new(1), true); // (1, F, T)

        // x0 variable point to the x0 bdd ptr
        let x0_var = mgr.var(VarLabel::new(3), true); // (3, F, T)
        let x0_ptr = mgr.var(VarLabel::new(4), true); // (4, F, T)

        // y_ptr is a bdd pointer to a disjunction: y = x || x0
        let y_ptr = mgr.or(x0_ptr, x_ptr); // (1, (4, F, T), T)

        // .. and y is a variable pointing to this y_ptr BDD
        let y_var = mgr.var(VarLabel::new(2), true); // (2, F, T)

        // observe on the variables x and y: x || y
        let observation = mgr.or(x_var, y_var); // (0, (2, F, T), T)

        // now the goal is to substitute out x and y variables for their BDD pointers:
        let x = mgr.compose(observation, VarLabel::new(2), y_ptr);
        // should result in: (0, (2, F, T), T) -> (0, (1, (4, F, T), T), T)
        // but it results in (1, (4, F, T), T)
        println!("{}", x.print_bdd());
        // composing again should be idempotent: there is no VarLabel(2) in the BDD:
        let x = mgr.compose(observation, VarLabel::new(2), y_ptr);
        // but it results in !(1, (4, F, T), T)
        println!("{}", x.print_bdd());
        // let printx = diagnostic_fold_dfs(
        //     &y_ptr,
        //     &mut |s: String, bdd| match bdd.node {
        //         BddPtr::PtrTrue => format!("T"),
        //         BddPtr::PtrFalse => format!("F"),

        //         BddPtr::Compl(n) => format!("!{}", s),
        //         BddPtr::Reg(n) => {
        //             format!("{:?}", bdd.node.var())
        //         }
        //     },
        //     "".to_string(),
        //     &|out| match (out.node, out.lo_hi) {
        //         (s, None) => s,
        //         (s, Some((lo, hi))) => format!("({}, {}, {})", s, lo, hi),
        //     },
        // );
        // println!("{}", printx);

        // let vs = diagnostic_fold_dfs(
        //     &y_ptr,
        //     &mut |vs: Vec<Option<VarLabel>>, bdd| {
        //         let mut vs = vs.clone();
        //         vs.push(bdd.node.var_safe());
        //         vs
        //     },
        //     vec![],
        //     &|out| match (out.node, out.lo_hi) {
        //         (s, None) => s,
        //         (s, Some((lo, hi))) => {
        //             let mut v = s.clone();
        //             v.extend(lo);
        //             v.extend(hi);
        //             v
        //         }
        //     },
        // );
        // println!("{:?}", vs);
        assert_eq!(
            x,
            y_expected,
            "{} != {}",
            x.print_bdd(),
            y_expected.print_bdd()
        );

        // let x = mgr.compose(observation, VarLabel::new(2), y_ptr);
        // let x = mgr.compose(observation, VarLabel::new(2), y_ptr);
        // assert_eq!(
        //     x,
        //     y_expected,
        //     "{} != {}",
        //     x.print_bdd(),
        //     y_expected.print_bdd()
        // );

        // observe bug 1: apply_substitutions becomes a negation
        // Comp. dist [(0, (2, F, T), T)]
        // substitutions:
        // - UniqueId(2)@(#2L2Ctrue): [(1, (4, F, T), T)]
        // - UniqueId(0)@(#0L0Ctrue): [(1, F, T)]
        // - UniqueId(3)@(#3L3Ctrue): [(4, F, T)]
        //
        // [substituting] notleaf: apply sub: UniqueId(2)@(1, (4, F, T), T)
        // [substituting] notleaf: transform: (0, (2, F, T), T) => (1, (4, F, T), T)
        // [substituting] notleaf: apply sub: UniqueId(2)@(1, (4, F, T), T)
        // [substituting] notleaf: transform: (1, (4, F, T), T) => !(1, T, (4, F, T))
        // [substituting] final: (0, (2, F, T), T) -> !(1, T, (4, F, T))
    }
}
