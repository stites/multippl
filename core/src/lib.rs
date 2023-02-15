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

    fn rendervec(fs: &Vec<String>) -> String {
        format!("[{}]", fs.join(", "))
    }
    fn fmt_f64(high_precision: bool) -> impl Fn(f64) -> String {
        if high_precision {
            move |x: f64| format!("{}", x)
        } else {
            move |x: f64| format!("{:.2}", x)
        }
    }
    fn debug_compiled(s: &str, w: &WeightMap, p: &SubstMap, c: &Compiled) {
        let renderw = |ws: &WeightMap| {
            ws.iter()
                .map(|(k, (l, h))| format!("{k}: ({l}, {h})"))
                .join(", ")
        };
        let renderp = |ps: &SubstMap| {
            ps.iter()
                .map(|(k, v)| format!("{k}: [{}]", v.iter().map(BddPtr::print_bdd).join(",")))
                .join(", ")
        };

        let dists = rendervec(
            &c.formulas
                .iter()
                .map(Formulas::dist)
                .map(|b: BddPtr| b.print_bdd())
                .collect_vec(),
        );

        let accepts = rendervec(
            &c.formulas
                .iter()
                .map(Formulas::accept)
                .map(|b: BddPtr| b.print_bdd())
                .collect_vec(),
        );

        let probs = rendervec(
            &c.probabilities
                .iter()
                .map(Probability::as_f64)
                .map(|x| fmt_f64(false)(x))
                .collect_vec(),
        );
        let weights = rendervec(
            &c.importance_weights
                .iter()
                .map(|x| fmt_f64(false)(*x))
                .collect_vec(),
        );
        debug!("{s}, [{}], [{}]", renderw(w), renderp(p));
        debug!("      \\||/  {}", dists);
        debug!("      \\||/  {}", accepts);
        debug!("      \\||/  {}", renderw(&c.weight_map));
        debug!("      \\||/  {}", renderp(&c.substitutions));
        debug!("      \\||/  {}", probs);
        debug!("      \\||/  {}", weights);
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
    pub struct Formulas {
        pub dist: BddPtr,
        pub accept: BddPtr,
    }
    impl Formulas {
        pub fn dist(&self) -> BddPtr {
            self.dist
        }
        pub fn accept(&self) -> BddPtr {
            self.accept
        }
    }

    #[derive(Debug, Clone)]
    pub struct Compiled {
        pub formulas: Vec<Formulas>,
        pub weight_map: WeightMap, // must be a hashmap as sample will collapse variables
        pub substitutions: SubstMap,
        pub probabilities: Vec<Probability>, // FIXME, can actually be removed -- just keeping this around for diagnostic purposes
        pub importance_weights: Vec<f64>,
    }
    impl Compiled {
        fn convex_combination(&self, o: &Compiled) -> Vec<f64> {
            izip!(
                &self.probabilities,
                &self.importance_weights,
                &o.probabilities,
                &o.importance_weights
            )
            .map(|(selfp, selfiw, op, oiw)| selfp.as_f64() * selfiw + op.as_f64() * oiw)
            .collect_vec()
        }
        fn default(dist: BddPtr, accept: BddPtr) -> Compiled {
            Compiled::default_vec(vec![Formulas { dist, accept }])
        }
        fn default_vec(formulas: Vec<Formulas>) -> Compiled {
            Compiled {
                formulas,
                weight_map: HashMap::new(),
                substitutions: HashMap::new(),
                probabilities: vec![Probability::new(1.0)],
                importance_weights: vec![1.0],
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
            ctx: &Γ,
            bl: &ANF,
            br: &ANF,
            m: &WeightMap,
            p: &SubstMap,
            op: &dyn Fn(&mut Mgr, BddPtr, BddPtr) -> BddPtr,
        ) -> Result<Compiled, CompileError> {
            let l = self.eval_anf(ctx, bl, m, p)?;
            let r = self.eval_anf(ctx, br, m, p)?;
            let mut weight_map = l.weight_map.clone();
            weight_map.extend(r.weight_map.clone());
            let mut substitutions = l.substitutions.clone();
            substitutions.extend(r.substitutions.clone());
            if l.formulas.len() != r.formulas.len() {
                return Err(SemanticsError(format!(
                    "impossible! compiled {} formulas on the left and {} formulas on the right.",
                    l.formulas.len(),
                    r.formulas.len()
                )));
            } else {
                let formulas = izip!(l.formulas, r.formulas)
                    .map(|(l, r)| {
                        assert!(l.accept == BddPtr::PtrTrue && r.accept == BddPtr::PtrTrue);
                        Formulas {
                            dist: op(self.mgr, l.dist, r.dist),
                            accept: BddPtr::PtrTrue,
                        }
                    })
                    .collect_vec();
                Ok(Compiled {
                    weight_map,
                    substitutions,
                    ..Compiled::default_vec(formulas)
                })
            }
        }

        pub fn eval_anf(
            &mut self,
            ctx: &Γ,
            a: &ANF,
            m: &WeightMap,
            p: &SubstMap,
        ) -> Result<Compiled, CompileError> {
            use ANF::*;
            match a {
                AVar(s, ty) => {
                    let (lbl, wm) = self.get_or_create_varlabel(s.to_string(), m, p);
                    if !ctx.typechecks_var(a, ty) {
                        Err(TypeError(format!(
                            "Expected {s} : {ty:?}\nGot: {a:?}\n{ctx:?}",
                        )))
                    } else {
                        Ok(Compiled {
                            substitutions: p.clone(),
                            weight_map: wm,
                            ..Compiled::default(self.mgr.var(lbl, true), BddPtr::PtrTrue)
                        })
                    }
                }
                AVal(Val::Bool(b)) => Ok(Compiled {
                    substitutions: p.clone(),
                    weight_map: m.clone(),
                    ..Compiled::default(BddPtr::from_bool(*b), BddPtr::PtrTrue)
                }),
                AVal(Val::Prod(pl, pr)) => Err(CompileError::Todo()),
                And(bl, br) => self.eval_anf_binop(ctx, bl, br, m, p, &BddManager::and),
                Or(bl, br) => self.eval_anf_binop(ctx, bl, br, m, p, &BddManager::or),
                Neg(bp) => {
                    let mut p = self.eval_anf(ctx, bp, m, p)?;
                    // FIXME negating a tuple? seems weird!!!!

                    p.formulas = p
                        .formulas
                        .into_iter()
                        .map(|f| Formulas {
                            dist: f.dist.neg(),
                            accept: f.accept,
                        })
                        .collect_vec();

                    Ok(p)
                }
            }
        }
        pub fn apply_substitutions(&mut self, bdd: BddPtr, p: &SubstMap) -> BddPtr {
            let mut fin = bdd;
            for (lbl, subs) in p.iter() {
                match subs[..] {
                    [sub] => fin = self.mgr.compose(fin, VarLabel::new(lbl.0), sub),
                    _ => continue,
                }
            }
            fin
        }
        pub fn log_samples(&mut self, id: UniqueId, ebound: &Expr, bound: &Compiled) {
            if ebound.is_sample() {
                let samples = bound
                    .formulas
                    .iter()
                    .map(Formulas::dist)
                    .map(|dist| match dist {
                        BddPtr::PtrTrue => true,
                        BddPtr::PtrFalse => false,
                        _ => panic!("impossible"),
                    })
                    .collect_vec();
                self.samples.insert(id, samples);
            }
        }

        pub fn eval_expr(
            &mut self,
            ctx: &Γ,
            e: &Expr,
            m: &WeightMap,
            p: &SubstMap,
        ) -> Result<Compiled, CompileError> {
            use Expr::*;
            match e {
                EAnf(a) => {
                    debug!(">>>anf: {:?}", a);
                    let c = self.eval_anf(ctx, a, m, p)?;
                    debug_compiled("anf", m, p, &c);
                    Ok(c)
                }
                EFst(a, ty) => {
                    todo!()
                    // debug!(">>>fst: {:?}", a);
                    // let c = self.eval_anf(ctx, a, m, p)?;
                    // debug_compiled("fst", m, p, &c);
                    // Ok(c)
                }
                ESnd(a, ty) => {
                    todo!()
                    // debug!(">>>snd: {:?}", a);
                    // let c = self.eval_anf(ctx, a, m, p)?;
                    // debug_compiled("snd", m, p, &c);
                    // Ok(c)
                }
                EProd(al, ar, ty) => {
                    // debug!(">>>prod: {:?} {:?}", al, ar);
                    // let mut left = self.eval_anf(ctx, al, m, p)?;
                    // let right = self.eval_anf(ctx, ar, m, p)?;

                    // debug_compiled("prod", m, p, &c);
                    // Ok(c)
                    todo!()
                }
                ELetIn(s, tbound, ebound, ebody, tbody) => {
                    debug!(">>>let-in {}", s);
                    let (lbl, wm) = self.get_or_create_varlabel(s.clone(), m, p);
                    let id = UniqueId(lbl.value());
                    let bound = self.eval_expr(ctx, ebound, &wm, p)?;

                    let mut bound_substitutions = bound.substitutions.clone();
                    bound_substitutions
                        .insert(id, bound.formulas.iter().map(Formulas::dist).collect_vec());

                    let newctx = ctx.append_var(s.to_string(), tbound);
                    let body =
                        self.eval_expr(&newctx, ebody, &bound.weight_map, &bound_substitutions)?;

                    let mut weight_map = bound.weight_map.clone();
                    weight_map.extend(body.weight_map);

                    let substitutions = body.substitutions.clone();
                    // substitutions.extend(body.substitutions);

                    let formulas = izip!(body.formulas.clone(), bound.formulas.clone())
                        .map(|(bodyf, boundf)| {
                            // NOTE: applying all substituting will normalize distributions in sample too much. This will cause
                            // samples to normalize in a way where we will fail to drop irrelevant structure
                            // old-code does this:
                            //
                            // let accept = self.mgr.and(bound.accept, body.accept);
                            // let accept = self.apply_substitutions(accept, &substitutions);
                            let accept = self.mgr.compose(bodyf.accept, lbl, boundf.accept);
                            let accept = self.mgr.and(boundf.accept, accept);
                            Formulas {
                                dist: self.apply_substitutions(bodyf.dist, &substitutions),
                                accept,
                            }
                        })
                        .collect_vec();

                    self.log_samples(id, ebound, &bound);

                    let probabilities = izip!(bound.probabilities, body.probabilities)
                        .map(|(p1, p2)| p1 * p2)
                        .collect_vec();
                    let importance_weights =
                        izip!(bound.importance_weights, body.importance_weights)
                            .map(|(p1, p2)| p1 * p2)
                            .collect_vec();

                    let c = Compiled {
                        formulas,
                        weight_map,
                        substitutions,
                        probabilities,
                        importance_weights,
                    };
                    debug_compiled(&format!("let-in {}", s), m, p, &c);
                    Ok(c)
                }
                EIte(cond, t, f, ty) => {
                    let pred = self.eval_anf(ctx, cond, &m.clone(), &p.clone())?;
                    let truthy = self.eval_expr(ctx, t, &m.clone(), &p.clone())?;
                    let falsey = self.eval_expr(ctx, f, &m.clone(), &p.clone())?;

                    let formulas = izip!(&pred.formulas, &truthy.formulas, &falsey.formulas)
                        .map(|(p, t, f)| {
                            let dist_l = self.mgr.and(p.dist, t.dist);
                            let dist_r = self.mgr.and(p.dist.neg(), f.dist);
                            let dist = self.mgr.or(dist_l, dist_r);

                            let accept_l = self.mgr.and(p.accept, t.accept);
                            let accept_r = self.mgr.and(p.accept.neg(), f.accept);
                            let accept = self.mgr.or(accept_l, accept_r);

                            Formulas { dist, accept }
                        })
                        .collect_vec();

                    let mut weight_map = truthy.weight_map.clone();
                    weight_map.extend(falsey.weight_map.clone());
                    let mut substitutions = truthy.substitutions.clone();
                    substitutions.extend(falsey.substitutions.clone());

                    let probabilities = izip!(&truthy.probabilities, &falsey.probabilities)
                        .map(|(t, f)| *t + *f)
                        .collect_vec();
                    let importance_weights = truthy.convex_combination(&falsey);
                    let c = Compiled {
                        formulas,
                        weight_map,
                        substitutions,
                        probabilities,
                        importance_weights,
                    };
                    debug_compiled("ite", m, p, &c);
                    Ok(c)
                }
                EFlip(param) => {
                    debug!(">>>flip {param}");
                    let sym = self.fresh();
                    let mut weight_map = m.clone();
                    let lbl = VarLabel::new(sym.0);

                    weight_map.insert(sym, (1.0 - *param, *param));
                    let c = Compiled {
                        formulas: vec![Formulas {
                            dist: self.mgr.var(lbl, true),
                            accept: BddPtr::PtrTrue,
                        }],
                        weight_map,
                        substitutions: p.clone(),
                        probabilities: vec![Probability::new(1.0)],
                        importance_weights: vec![1.0],
                    };
                    debug_compiled("flip {param}", m, p, &c);
                    Ok(c)
                }
                EObserve(a) => {
                    debug!(">>>observe");
                    let comp = self.eval_anf(ctx, a, m, p)?;
                    // FIXME: I don't think there is a need to apply substitutions here, but doing this doesn't hurt
                    let apply_subs = |d| self.apply_substitutions(d, p);
                    let dists = comp
                        .formulas
                        .iter()
                        .map(Formulas::dist)
                        .map(apply_subs)
                        .collect_vec();

                    let formulas = dists
                        .clone()
                        .into_iter()
                        .map(|dist| Formulas {
                            dist: BddPtr::PtrTrue,
                            accept: dist,
                        })
                        .collect_vec();

                    let probabilities = vec![Probability::new(1.0); dists.len()];
                    let importance_weights = dists
                        .into_iter()
                        .map(|dist| {
                            let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                            let var_order = VarOrder::linear_order(max_var as usize);
                            dist.wmc(&var_order, &wmc_params)
                        })
                        .collect_vec();

                    let c = Compiled {
                        formulas,
                        weight_map: m.clone(),
                        substitutions: p.clone(),
                        probabilities,
                        importance_weights,
                    };
                    debug_compiled("observe", m, p, &c);
                    Ok(c)
                }
                ESample(e) => {
                    debug!(">>>sample");
                    let comp = self.eval_expr(ctx, e, m, p)?;

                    if comp.formulas.iter().any(|f| f.accept != BddPtr::PtrTrue) {
                        return Err(SemanticsError(
                            "impossible! Sample statement includes observe statement.".to_string(),
                        ));
                    }
                    let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                    let var_order = VarOrder::linear_order(max_var as usize);
                    let (qs, formulas): (Vec<Probability>, Vec<Formulas>) = comp
                        .formulas
                        .iter()
                        .map(|f| {
                            let theta_q = f.dist.wmc(&var_order, &wmc_params) as f64;
                            let bern = Bernoulli::new(theta_q).unwrap();
                            let sample = bern.sample(self.rng);
                            let q = Probability::new(if sample { theta_q } else { 1.0 - theta_q });
                            let formulas = Formulas {
                                dist: BddPtr::from_bool(sample),
                                accept: self.mgr.iff(f.dist, BddPtr::PtrTrue),
                            };
                            (q, formulas)
                        })
                        .unzip();

                    let c = Compiled {
                        formulas,
                        weight_map: comp.weight_map.clone(),
                        substitutions: p.clone(),
                        probabilities: qs,
                        importance_weights: vec![1.0; comp.formulas.len()],
                    };
                    debug_compiled("sample", m, p, &c);
                    Ok(c)
                }
            }
        }
    }
    pub fn compile(env: &mut Env, p: &Program) -> Result<Compiled, CompileError> {
        match p {
            Program::Body(e) => {
                debug!("========================================================");
                env.eval_expr(
                    &Default::default(),
                    e,
                    &Default::default(),
                    &Default::default(),
                )
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

    #[test]
    // #[traced_test]
    fn tuple0() {
        let mk = |ret: Expr| {
            Program::Body(lets![
                "x" ; B!() ;= flip!(1.0/3.0);
                "y" ; B!() ;= flip!(1.0/4.0);
                ...? ret ; B!()
            ])
        };
        check_exact("p02/y,x", vec![3.0 / 12.0, 4.0 / 12.0], &mk02(b!("y", "x")));
    }

    // free variable edge case
    #[test]
    #[ignore]
    #[traced_test]
    fn free_variables_0() {
        let mk = |ret: Expr| {
            Program::Body(lets![
               "x" ; B!() ;= flip!(1/3);
               "y" ; B!() ;= sample!(var!("x"));
               ...? ret ; B!()
            ])
        };
        check_approx("free/x ", 1.0 / 3.0, &mk(var!("x")), 1000);
        // FIXME: still broken! getting 1/2 instead of 1/3
        check_approx("free/y ", 1.0 / 3.0, &mk(var!("y")), 1000);
    }
    // #[test]
    // // #[traced_test]
    // fn free_variables_1() {
    //     // FIXME: This seems like a huge problem!
    //     let problem = {
    //         Program::Body(lets![
    //            "x" := flip!(1/3);
    //            "l" := sample!(var!("x"));
    //            "_" := observe!(var!("x"));
    //            ...? var!("l")
    //         ])
    //     };
    //     check_approx("free/!!", 1.0 / 1.0, &problem, 1000);
    // }
    // #[test]
    // // #[traced_test]
    // fn free_variables_2() {
    //     let mk = |ret: Expr| {
    //         Program::Body(lets![
    //             "x" := flip!(1/3);
    //             "l" := sample!(
    //                 lets![
    //                     "x0" := flip!(1/5);
    //                     ...? or!("x0", "x")
    //                 ]);
    //            "_" := observe!(var!("x")); // is this a problem?
    //            ...? ret
    //         ])
    //     };
    //     check_approx("free/?1", 1.0 / 1.0, &mk(var!("x")), 1000);
    //     check_approx("free/?2", 1.0 / 1.0, &mk(var!("l")), 1000);
    // }

    // // free variable edge case
    // #[test]
    // // #[traced_test]
    // fn free_variables_shared() {
    //     let p = Program::Body(lets![
    //        "x" := flip!(1/3);
    //        "l" := sample!(var!("x"));
    //        "r" := sample!(var!("x"));
    //        ...? and!("x", "y")
    //     ]);
    //     check_approx("shared ", 1.0 / 3.0, &p, 1000);
    // }

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
