#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)] // for Gamma : )
// temporary as I convert to using types
#![allow(unused_variables)]
#![allow(clippy::clone_on_copy)]
#![allow(clippy::type_complexity)]
#![allow(clippy::redundant_clone)]
#![allow(clippy::useless_format)]
#![allow(clippy::needless_return)]
#![allow(clippy::upper_case_acronyms)]
#![allow(clippy::single_component_path_imports)]
#![allow(clippy::enum_variant_names)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::let_and_return)]
#![allow(clippy::len_zero)]
#![allow(clippy::assign_op_pattern)]
#![allow(clippy::unnecessary_cast)]
#![allow(clippy::unnecessary_lazy_evaluations)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::ptr_arg)]
use itertools::*;
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
use std::collections::HashMap;
use std::string::String;
use tracing::debug;

mod grammar;

mod inference;
mod parser;
#[cfg(test)]
mod tests;
// use grammar::*;

pub mod semantics {
    use super::*;
    use grammar::*;
    use rand::distributions::{Bernoulli, Distribution};
    use rand::rngs::StdRng;
    use rand::SeedableRng;
    use rsdd::builder::cache::*;
    use rsdd::repr::bdd::*;
    use rsdd::repr::ddnnf::*;
    use rsdd::repr::var_label::*;
    use rsdd::repr::var_order::VarOrder;
    use rsdd::repr::wmc::WmcParams;
    use rsdd::sample::probability::Probability;
    use std::fmt;
    type Mgr = BddManager<AllTable<BddPtr>>;

    pub fn rendervec(fs: &Vec<String>) -> String {
        format!("[{}]", fs.join(", "))
    }

    pub fn renderfloats(fs: &Vec<f64>, high_prec: bool) -> String {
        rendervec(&fs.iter().map(|x| fmt_f64(high_prec)(*x)).collect_vec())
    }

    pub fn renderbdds(fs: &Vec<BddPtr>) -> String {
        rendervec(&fs.iter().map(|b| b.print_bdd()).collect_vec())
    }
    pub fn fmt_f64(high_precision: bool) -> impl Fn(f64) -> String {
        if high_precision {
            move |x: f64| format!("{}", x)
        } else {
            move |x: f64| format!("{:.2}", x)
        }
    }
    fn var_node(bdd: BddPtr) -> Option<VarLabel> {
        let n = bdd.into_node_safe()?;
        if (n.low == BddPtr::PtrTrue && n.high == BddPtr::PtrFalse)
            || (n.low == BddPtr::PtrFalse && n.high == BddPtr::PtrTrue)
        {
            Some(n.var)
        } else {
            None
        }
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
                .map(|(k, v)| format!("{k}: {}", renderbdds(v)))
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

    #[derive(Clone, Copy, Eq, Hash, PartialEq, Debug)]
    pub struct UniqueId(u64);
    impl UniqueId {
        pub fn from_lbl(lbl: VarLabel) -> UniqueId {
            UniqueId(lbl.value())
        }
        pub fn as_lbl(&self) -> VarLabel {
            VarLabel::new(self.0)
        }
    }

    impl fmt::Display for UniqueId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "#{}", self.0)
        }
    }

    pub type WeightMap = HashMap<UniqueId, (f64, f64)>;
    pub type SubstMap = HashMap<UniqueId, Vec<BddPtr>>;

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
                selfp.as_f64() * self.importance_weight + op.as_f64() * o.importance_weight
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
        pub gensym: u64,
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
                gensym: 0,
                mgr,
                rng,
                samples: HashMap::new(),
            }
        }
    }

    pub struct Env<'a> {
        pub names: HashMap<String, UniqueId>,
        pub gensym: u64,
        pub mgr: &'a mut BddManager<AllTable<BddPtr>>,
        pub rng: &'a mut StdRng,
        pub samples: HashMap<UniqueId, Vec<bool>>,
    }
    impl<'a> Env<'a> {
        pub fn new(mgr: &'a mut BddManager<AllTable<BddPtr>>, rng: &'a mut StdRng) -> Env<'a> {
            Env {
                names: HashMap::new(),
                gensym: 0,
                mgr,
                rng,
                samples: HashMap::new(),
            }
        }
        pub fn reset_names(&mut self) {
            self.gensym = 0;
            self.names = HashMap::new();
        }
        pub fn from_args(x: &'a mut EnvArgs) -> Env<'a> {
            Env {
                names: x.names.clone(),
                gensym: x.gensym.clone(),
                mgr: &mut x.mgr,
                rng: &mut x.rng,
                samples: HashMap::new(),
            }
        }
        fn _fresh(&mut self, ovar: Option<String>) -> UniqueId {
            let sym = self.gensym;
            self.gensym += 1;
            let var = ovar.unwrap_or(format!("_{sym}"));
            self.names.insert(var, UniqueId(sym));
            UniqueId(sym)
        }
        fn fresh(&mut self) -> UniqueId {
            self._fresh(None)
        }
        fn get_var(&self, var: String) -> Option<UniqueId> {
            self.names.get(&var).copied()
        }
        fn get_or_create(&mut self, var: String) -> UniqueId {
            let osym = self.get_var(var.clone());
            osym.unwrap_or_else(|| self._fresh(Some(var)))
        }
        /// a complicated mess
        fn get_or_create_varlabel(
            &mut self,
            s: String,
            m: &WeightMap,
            p: &SubstMap,
        ) -> (VarLabel, WeightMap) {
            match self.get_var(s.clone()) {
                None => {
                    let id = self._fresh(Some(s.clone()));
                    let lbl = VarLabel::new(id.0);
                    let mut mm = m.clone();
                    mm.insert(id, const_weight());
                    (lbl, mm)
                }
                Some(sym) => unsafe {
                    let mut nxt = p.get(&sym);
                    while nxt.is_some() {
                        match nxt.unwrap()[..] {
                            [BddPtr::Reg(n)] => {
                                if ((*n).low == BddPtr::PtrTrue && (*n).high == BddPtr::PtrFalse)
                                    || ((*n).low == BddPtr::PtrFalse
                                        && (*n).high == BddPtr::PtrTrue)
                                {
                                    nxt = p.get(&UniqueId((*n).var.value()));
                                } else {
                                    break;
                                }
                            }
                            _ => break,
                        }
                    }
                    match nxt {
                        Some(vs) => match vs[..] {
                            [BddPtr::Reg(n)] => ((*n).var.clone(), m.clone()),
                            _ => {
                                let lbl = VarLabel::new(sym.0);
                                (lbl, m.clone())
                            }
                        },
                        _ => {
                            let lbl = VarLabel::new(sym.0);
                            (lbl, m.clone())
                        }
                    }
                },
            }
        }
        pub fn eval_anf_binop(
            &mut self,
            ctx: &Context,
            bl: &ANF,
            br: &ANF,
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

        pub fn eval_anf(&mut self, ctx: &Context, a: &ANF) -> Result<Compiled, CompileError> {
            use ANF::*;
            match a {
                AVar(s, ty) => {
                    let (lbl, wm) = self.get_or_create_varlabel(
                        s.to_string(),
                        &ctx.weight_map,
                        &ctx.substitutions,
                    );
                    if !ctx.gamma.typechecks(s.clone(), ty) {
                        Err(TypeError(format!(
                            "Expected {s} : {ty:?}\nGot: {a:?}\n{ctx:?}",
                        )))
                    } else {
                        Ok(Compiled {
                            accept: ctx.accept.clone(),
                            substitutions: ctx.substitutions.clone(),
                            weight_map: wm,
                            ..Compiled::default(self.mgr.var(lbl, true))
                        })
                    }
                }
                AVal(Val::Bool(b)) => Ok(Compiled {
                    accept: ctx.accept.clone(),
                    substitutions: ctx.substitutions.clone(),
                    weight_map: ctx.weight_map.clone(),
                    ..Compiled::default(BddPtr::from_bool(*b))
                }),
                AVal(Val::Prod(vs)) => Err(CompileError::Todo()),
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

        // TODO: make sure substitutions are fully normalized? I don't think this will ever be a problem.
        pub fn apply_substitutions1(&mut self, bdd: BddPtr, p: &SubstMap) -> Vec<BddPtr> {
            match var_node(bdd) {
                Some(lbl) => match p.get(&UniqueId::from_lbl(lbl)) {
                    None => vec![bdd],
                    Some(subs) => subs.clone(),
                },
                None => {
                    let finl = p.iter().fold(bdd, |fin, (lbl, sub)| {
                        if sub.len() > 1 {
                            fin // subs are intended to be a product and should be replacing a variable.
                        } else {
                            self.mgr.compose(fin, lbl.as_lbl(), sub[0])
                        }
                    });
                    vec![finl]
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

        pub fn log_samples(&mut self, id: UniqueId, ebound: &Expr, bound: &Compiled) {
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

        pub fn eval_expr(&mut self, ctx: &Context, e: &Expr) -> Result<Compiled, CompileError> {
            use Expr::*;
            match e {
                EAnf(a) => {
                    debug!(">>>anf: {:?}", a);
                    let c = self.eval_anf(ctx, a)?;
                    debug_compiled("anf", ctx, &c);
                    Ok(c)
                }
                EPrj(i, a, ty) => {
                    if i > &1 {
                        debug!(">>>prj@{}: {:?}", i, a);
                    }
                    // ignore types for now.
                    // let aty = a.as_type();
                    // assert!(aty.left() == Some(*ty.clone()), "actual {:?} != expected {:?}. type is: {:?}", aty.left(), Some(*ty.clone()), ty);
                    let mut c = self.eval_anf(ctx, a)?;
                    let dists = self.apply_substitutions(c.dists, &ctx.substitutions);
                    c.dists = vec![dists[*i]];
                    if i > &1 {
                        debug_compiled(&format!("prj@{}", i).to_string(), ctx, &c);
                    }
                    Ok(c)
                }
                EFst(a, ty) => {
                    debug!(">>>fst: {:?}", a);
                    let c = self.eval_expr(ctx, &EPrj(0, a.clone(), ty.clone()))?;
                    debug_compiled("fst", ctx, &c);
                    Ok(c)
                }
                ESnd(a, ty) => {
                    debug!(">>>snd: {:?}", a);
                    let c = self.eval_expr(ctx, &EPrj(1, a.clone(), ty.clone()))?;
                    debug_compiled("snd", ctx, &c);
                    Ok(c)
                }
                EProd(anfs, ty) => {
                    debug!(">>>prod: {:?} {:?}", anfs, ty);
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
                ELetIn(s, tbound, ebound, ebody, tbody) => {
                    debug!(">>>let-in {}", s);
                    let (lbl, wm) =
                        self.get_or_create_varlabel(s.clone(), &ctx.weight_map, &ctx.substitutions);
                    let id = UniqueId(lbl.value());

                    let mut newctx = ctx.clone();
                    newctx.weight_map = wm;

                    let bound = self.eval_expr(&newctx, ebound)?;
                    let mut bound_substitutions = bound.substitutions.clone();
                    bound_substitutions.insert(id, bound.dists.clone());

                    let mut newctx = newctx.clone();
                    newctx.weight_map = bound.weight_map.clone();
                    newctx.substitutions = bound_substitutions.clone();
                    newctx.gamma = ctx.gamma.append(s.clone(), tbound);

                    let body = self.eval_expr(&newctx, ebody)?;
                    let mut weight_map = bound.weight_map.clone();
                    weight_map.extend(body.weight_map);

                    let mut substitutions = body.substitutions.clone();
                    substitutions.extend(bound_substitutions); // FIXME: almost certainly redundant
                                                               // println!("{:?}", &comp.weight_map);
                                                               // println!("{:?}", wmc_params);
                                                               // println!("accept: {}", accept.print_bdd());

                    let dists = self.apply_substitutions(body.dists, &substitutions);
                    let accept = self.mgr.and(bound.accept, body.accept);
                    let accept = self.mgr.and(accept, ctx.accept);

                    self.log_samples(id, ebound, &bound);

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
                EIte(cond, t, f, ty) => {
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
                            "Expected both branches of ITE to return same type\nGot (left): {:?}\nGot (right):{:?}",
                            t.as_type(),
                            f.as_type()
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
                        .map(|(t, f)| *t + *f)
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
                EFlip(param) => {
                    debug!(">>>flip {param}");
                    let sym = self.fresh();
                    let mut weight_map = ctx.weight_map.clone();
                    let lbl = VarLabel::new(sym.0);
                    weight_map.insert(sym, (1.0 - *param, *param));

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
                EObserve(a) => {
                    debug!(">>>observe");
                    let comp = self.eval_anf(ctx, a)?;
                    let dists = self
                        .apply_substitutions(comp.dists, &ctx.substitutions)
                        .into_iter()
                        .fold(BddPtr::PtrTrue, |global, cur| self.mgr.and(global, cur));

                    let accept = dists;
                    let accept = self.mgr.and(accept, ctx.accept);

                    let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                    let var_order = VarOrder::linear_order(max_var as usize);
                    let importance_weight = accept.wmc(&var_order, &wmc_params);

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
                ESample(e) => {
                    debug!(">>>sample");
                    let comp = self.eval_expr(ctx, e)?;

                    if comp.accept != BddPtr::PtrTrue {
                        return Err(SemanticsError(
                            "impossible! Sample statement includes observe statement.".to_string(),
                        ));
                    }
                    let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                    let var_order = VarOrder::linear_order(max_var as usize);
                    let comp_dists = self.apply_substitutions(comp.dists, &ctx.substitutions);

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

                    // FIXME(#1): adding this breaks tuple support (unless we bring in data-flow analysis)
                    // but without it we cannot satisfy circumstances like `free_variables_0`
                    //
                    // debug!(dists = renderbdds(&dists));
                    // let accept = comp_dists.iter().fold(comp.accept.clone(), |global, dist| {
                    //     let dist_holds = self.mgr.iff(*dist, BddPtr::PtrTrue);
                    //     self.mgr.and(global, dist_holds)
                    // });
                    // let accept = self.mgr.and(accept, ctx.accept);
                    let accept = ctx.accept;
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
    pub fn compile(env: &mut Env, p: &Program) -> Result<Compiled, CompileError> {
        match p {
            Program::Body(e) => {
                debug!("========================================================");
                env.eval_expr(&Default::default(), e)
            }
        }
    }
}

#[cfg(test)]
mod active_tests {
    use super::semantics::*;
    use super::*;
    use grammar::*;
    use inference::*;
    use tests::*;
    use tracing_test::traced_test;

    // #[test]
    // // #[traced_test]
    // fn nested_1() {
    //     let mk = |ret: Expr| {
    //         Program::Body(lets![
    //             "x" := sample!(sample!(flip!(1/3)));
    //             "y" := flip!(1/4);
    //             "_" := observe!(or!("x", "y"));
    //             ...? ret
    //         ])
    //     };
    //     check_approx("nest/y  ", 3.0 / 6.0, &mk(var!("y")), 10000);
    //     check_approx("nest/x  ", 4.0 / 6.0, &mk(var!("x")), 10000);
    //     check_approx("nest/x|y", 6.0 / 6.0, &mk(or!("x", "y")), 10000);
    //     check_approx("nest/x&y", 1.0 / 6.0, &mk(and!("x", "y")), 10000);
    // }

    // #[test]
    // // #[traced_test]
    // fn nested_2() {
    //     let mk = |ret: Expr| {
    //         Program::Body(lets![
    //             "a" := flip!(1/5);
    //             "b" := sample!(
    //                 lets![
    //                     "x" := sample!(flip!(1/3));
    //                     "y" := flip!(1/4);
    //                     ...? or!("x", "y")
    //                 ]);
    //             "_" := observe!(or!("a", "b")); // is this a problem?
    //             ...? ret
    //         ])
    //     };
    //     check_approx("nest/y  ", 3.0 / 6.0, &mk(var!("y")), 10000);
    //     check_approx("nest/x  ", 4.0 / 6.0, &mk(var!("x")), 10000);
    //     check_approx("nest/x|y", 6.0 / 6.0, &mk(or!("x", "y")), 10000);
    //     check_approx("nest/x&y", 1.0 / 6.0, &mk(and!("x", "y")), 10000);
    // }

    // #[test]
    // // #[traced_test]
    // fn ite_1() {
    //     let mk = |ret: Expr| {
    //         Program::Body(lets![
    //             "a" := flip!(1/3);
    //             "b" := ite!(
    //                 if ( var!("a") )
    //                 then { flip!(1/4) }
    //                 else { flip!(1/5) });
    //             "_" := observe!(or!("a", "b")); // is this a problem?
    //             ...? ret
    //         ])
    //     };
    //     check_approx("ite1/y  ", 6.0 / 6.0, &mk(var!("y")), 10000);
    //     check_approx("ite1/x  ", 6.0 / 6.0, &mk(var!("x")), 10000);
    //     check_approx("ite1/x|y", 6.0 / 6.0, &mk(or!("x", "y")), 10000);
    //     check_approx("ite1/x&y", 6.0 / 6.0, &mk(and!("x", "y")), 10000);
    // }

    // #[test]
    // // #[traced_test]
    // fn ite_2_with_one_sample() {
    //     let mk = |ret: Expr| {
    //         Program::Body(lets![
    //             "a" := flip!(1/3);
    //             "b" := ite!(
    //                 if ( var!("a") )
    //                 then { sample!(flip!(1/4)) }
    //                 else {         flip!(1/5)  });
    //             "_" := observe!(or!("a", "b")); // is this a problem?
    //             ...? ret
    //         ])
    //     };
    //     check_approx("ite2/y  ", 6.0 / 6.0, &mk(var!("y")), 10000);
    //     check_approx("ite2/x  ", 6.0 / 6.0, &mk(var!("x")), 10000);
    //     check_approx("ite2/x|y", 6.0 / 6.0, &mk(or!("x", "y")), 10000);
    //     check_approx("ite2/x&y", 6.0 / 6.0, &mk(and!("x", "y")), 10000);
    // }

    // /// a directed 2x2 grid test where we place samples according to various policies
    // ///   (0,0) -> (0,1)
    // ///     v        v
    // ///   (1,0) -> (1,1)
    // #[test]
    // // #[traced_test]
    // fn grid2x2() {
    //     let mk = |ret: Expr| {
    //         Program::Body(lets![
    //             "0_0" := flip!(1/2);
    //             "path" := ite!( ( var!("0_0") ) ?
    //                 ( lets![ "0_1" := flip!(1/3); ...? var!("0_1") ] ) :
    //                 ( lets![ "1_0" := flip!(1/4); ...? var!("1_0") ] )
    //                 );
    //             "1_1" := flip!(1/5);
    //             "_" := observe!(or!("0_0", "path", "1_1", "x", "x")); // is this a problem?
    //             ...? ret
    //         ])
    //     };
    //     check_approx("ite2/y  ", 6.0 / 6.0, &mk(var!("y")), 10000);
    //     check_approx("ite2/x  ", 6.0 / 6.0, &mk(var!("x")), 10000);
    //     check_approx("ite2/x|y", 6.0 / 6.0, &mk(or!("x", "y")), 10000);
    //     check_approx("ite2/x&y", 6.0 / 6.0, &mk(and!("x", "y")), 10000);
    // }

    // /// a directed 3x3 grid test where we place samples according to various policies
    // ///   (0,0) -> (0,1) -> (0,2)
    // ///     v        v        v
    // ///   (1,0) -> (1,1) -> (1,2)
    // ///     v        v        v
    // ///   (2,0) -> (2,1) -> (2,2)
    // #[test]
    // // #[traced_test]
    // fn grid3x3() {
    //     todo!()
    // }
}
