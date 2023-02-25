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

pub type Mgr = BddManager<AllTable<BddPtr>>;

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

#[derive(Clone, PartialEq, Debug)]
pub struct Weight {
    pub lo: f64,
    pub hi: f64,
}
impl Weight {
    pub fn as_tuple(&self) -> (f64, f64) {
        (self.lo, self.hi)
    }
    pub fn from_high(hi: f64) -> Weight {
        Weight { lo: 1.0 - hi, hi }
    }
    pub fn constant() -> Weight {
        Weight { lo: 1.0, hi: 1.0 }
        // Self::from_high(0.5)
    }
}

#[derive(Debug, Clone)]
pub struct WeightMap {
    pub weights: HashMap<VarLabel, Weight>,
}

impl WeightMap {
    pub fn as_params(&self, max_label: u64) -> WmcParams<f64> {
        let mut wmc_params = WmcParams::new(0.0, 1.0);
        let cnst = Weight::constant();
        for ix in 0..max_label {
            let lbl = VarLabel::new(ix);
            let weight = self.weights.get(&lbl).unwrap_or_else(|| &cnst);
            wmc_params.set_weight(lbl, weight.lo, weight.hi);
        }
        for (label, weight) in self.weights.iter() {
            wmc_params.set_weight(*label, weight.lo, weight.hi);
        }
        wmc_params
    }
    pub fn insert(&mut self, lbl: VarLabel, high: f64) {
        self.weights.insert(lbl, Weight::from_high(high));
    }
}
impl Default for WeightMap {
    fn default() -> Self {
        Self {
            weights: HashMap::new(),
        }
    }
}

impl IntoIterator for WeightMap {
    type Item = (VarLabel, Weight);
    type IntoIter = std::collections::hash_map::IntoIter<VarLabel, Weight>;

    fn into_iter(self) -> Self::IntoIter {
        self.weights.into_iter()
    }
}

pub type SubstMap = HashMap<UniqueId, (Vec<BddPtr>, Var)>;

#[derive(Debug, Clone)]
pub struct Context {
    pub accept: BddPtr,
    pub substitutions: SubstMap,
    pub weightmap: WeightMap,
}
impl Context {
    pub fn from_compiled(c: &Compiled) -> Self {
        Context {
            accept: c.accept.clone(),
            substitutions: c.substitutions.clone(),
            weightmap: c.weightmap.clone(),
        }
    }
}
impl Default for Context {
    fn default() -> Self {
        Context {
            accept: BddPtr::PtrTrue,
            substitutions: Default::default(),
            weightmap: Default::default(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Compiled {
    pub dists: Vec<BddPtr>,
    pub accept: BddPtr,
    pub probabilities: Vec<Probability>,
    pub weightmap: WeightMap,
    pub substitutions: SubstMap,
    pub importance_weight: f64,
}
impl Compiled {
    fn convex_combination(&self, o: &Compiled) -> f64 {
        izip!(&self.probabilities, &o.probabilities,).fold(0.0, |res, (selfp, op)| {
            (selfp.as_f64() * self.importance_weight + op.as_f64() * o.importance_weight) / 2.0
        })
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
    ) -> Result<Compiled, CompileError> {
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
                importance_weight: 1.0,
            })
        }
    }
    pub fn eval_anf(&mut self, ctx: &Context, a: &AnfAnn) -> Result<Compiled, CompileError> {
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
                    importance_weight: 1.0,
                }),
            },
            AVal(_, Val::Bool(b)) => Ok(Compiled {
                dists: vec![BddPtr::from_bool(*b)],
                accept: ctx.accept.clone(),
                substitutions: ctx.substitutions.clone(),
                weightmap: ctx.weightmap.clone(),
                probabilities: vec![Probability::new(1.0)],
                importance_weight: 1.0,
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
                let mut c = self.eval_anf(ctx, a)?;
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
                    weightmap: ctx.weightmap.clone(),
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

                let bound = self.eval_expr(&ctx, ebound)?;
                let mut newctx = Context::from_compiled(&bound);
                newctx
                    .substitutions
                    .insert(var.id, (bound.dists.clone(), var.clone()));

                let body = self.eval_expr(&newctx, ebody)?;

                let accept = self.mgr.and(body.accept, ctx.accept);

                self.log_samples(var.id, ebound, &bound);

                let probabilities = izip!(bound.probabilities, body.probabilities)
                    .map(|(p1, p2)| p1 * p2)
                    .collect_vec();
                let importance_weight = bound.importance_weight * body.importance_weight;

                let c = Compiled {
                    dists: body.dists,
                    accept,
                    substitutions: body.substitutions.clone(),
                    weightmap: body.weightmap.clone(),
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

                let mut substitutions = truthy.substitutions.clone();
                substitutions.extend(falsey.substitutions.clone());
                let mut weightmap = truthy.weightmap.clone();
                weightmap.weights.extend(falsey.weightmap.clone());

                let probabilities = izip!(&truthy.probabilities, &falsey.probabilities)
                    // dancing with the numerically unstable devil
                    .map(|(t, f)| (*t * Probability::new(0.5) + *f * Probability::new(0.5)))
                    .collect_vec();
                let importance_weight = truthy.convex_combination(&falsey);
                let c = Compiled {
                    dists,
                    accept,
                    weightmap,
                    substitutions,
                    probabilities,
                    importance_weight,
                };
                debug_compiled("ite", ctx, &c);
                Ok(c)
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
                    importance_weight: 1.0,
                };
                debug_compiled("flip", ctx, &c);
                Ok(c)
            }
            EObserve(_, a) => {
                debug!(">>>observe");
                let comp = self.eval_anf(ctx, a)?;
                debug!("[observe] In. Accept {}", &ctx.accept.print_bdd());
                debug!("[observe] Comp. dist {}", renderbdds(&comp.dists));
                debug!("[observe] weightmap  {:?}", ctx.weightmap);
                let accept = comp
                    .dists
                    .into_iter()
                    .fold(ctx.accept.clone(), |global, cur| self.mgr.and(global, cur));

                let wmc_params = ctx.weightmap.as_params(self.max_label.unwrap());
                let var_order = self.order.clone().unwrap();
                let avars = crate::utils::variables(accept);
                for (i, var) in avars.iter().enumerate() {
                    debug!("{}@{:?}: {:?}", i, var, wmc_params.get_var_weight(*var));
                }
                // debug!("[observe] max_var    {}", max_var);
                debug!("[observe] WMCParams  {:?}", wmc_params);
                debug!("[observe] VarOrder   {:?}", var_order);
                debug!("[observe] Accept     {}", accept.print_bdd());
                let importance_weight = accept.wmc(&var_order, &wmc_params);
                debug!("[observe] IWeight    {}", importance_weight);

                let c = Compiled {
                    dists: vec![BddPtr::PtrTrue],
                    accept,
                    weightmap: ctx.weightmap.clone(),
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
                    weightmap: ctx.weightmap.clone(),
                    substitutions: ctx.substitutions.clone(), // any dangling references will be treated as constant and, thus, ignored -- information about this will live only in the propagated weight
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
