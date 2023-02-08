#![allow(dead_code)]
use itertools::*;
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
use std::collections::HashMap;
use std::string::String;
use tracing::debug;

mod grammar;
mod inference;
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

    #[derive(Clone, Copy, Eq, Hash, PartialEq, Debug)]
    pub struct UniqueId(u64);

    impl fmt::Display for UniqueId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "#{}", self.0)
        }
    }

    type WeightMap = HashMap<UniqueId, (f32, f32)>;
    type SubstMap = HashMap<UniqueId, BddPtr>;

    fn const_weight() -> (f32, f32) {
        (0.5, 0.5)
    }
    pub fn weight_map_to_params(m: &WeightMap) -> (WmcParams<f32>, u64) {
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
    pub struct Compiled {
        pub dist: BddPtr,
        pub accept: BddPtr,
        pub weight_map: WeightMap, // must be a hashmap as sample will collapse variables
        pub substitutions: SubstMap,
        pub probability: Probability,
        pub importance_weight: f64,
    }
    impl Compiled {
        fn convex_combination(&self, o: &Compiled) -> f64 {
            self.probability.as_f64() * self.importance_weight
                + o.probability.as_f64() * o.importance_weight
        }
    }

    impl Default for Compiled {
        fn default() -> Compiled {
            Compiled {
                dist: BddPtr::PtrTrue,
                accept: BddPtr::PtrTrue,
                weight_map: HashMap::new(),
                substitutions: HashMap::new(),
                probability: Probability::new(1.0),
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
            }
        }
    }

    pub struct Env<'a> {
        pub names: HashMap<String, UniqueId>,
        pub gensym: u64,
        pub mgr: &'a mut BddManager<AllTable<BddPtr>>,
        pub rng: &'a mut StdRng,
    }
    impl<'a> Env<'a> {
        pub fn new(mgr: &'a mut BddManager<AllTable<BddPtr>>, rng: &'a mut StdRng) -> Env<'a> {
            Env {
                names: HashMap::new(),
                gensym: 0,
                mgr,
                rng,
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
                    let renderp = |ps: &HashMap<String, UniqueId>| {
                        ps.iter().map(|(k, v)| format!("{k}:{v}")).join(", ")
                    };

                    let id = self._fresh(Some(s.clone()));
                    let lbl = VarLabel::new(id.0);
                    let mut mm = m.clone();
                    mm.insert(id, const_weight());
                    (lbl, mm)
                }
                Some(sym) => unsafe {
                    let mut nxt = p.get(&sym);
                    while nxt.is_some() {
                        match nxt {
                            Some(BddPtr::Reg(n)) => {
                                if ((**n).low == BddPtr::PtrTrue && (**n).high == BddPtr::PtrFalse)
                                    || ((**n).low == BddPtr::PtrFalse
                                        && (**n).high == BddPtr::PtrTrue)
                                {
                                    nxt = p.get(&UniqueId((**n).var.value().clone()));
                                } else {
                                    break;
                                }
                            }
                            _ => break,
                        }
                    }
                    match nxt {
                        Some(BddPtr::Reg(n)) => {
                            let mut nn = n.clone();
                            ((**n).var.clone(), m.clone())
                        }
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
            bl: ANF,
            br: ANF,
            m: &WeightMap,
            p: &SubstMap,
            op: &dyn Fn(&mut BddManager<AllTable<BddPtr>>, BddPtr, BddPtr) -> BddPtr,
        ) -> Compiled {
            let l = self.eval_anf(bl, m, p);
            let r = self.eval_anf(br, m, p);
            let mut weight_map = l.weight_map.clone();
            weight_map.extend(r.weight_map.clone());
            let mut substitutions = l.substitutions.clone();
            substitutions.extend(r.substitutions.clone());
            let dist = op(self.mgr, l.dist, r.dist);
            Compiled {
                dist,
                weight_map,
                substitutions,
                ..Default::default()
            }
        }
        pub fn eval_anf(&mut self, a: ANF, m: &WeightMap, p: &SubstMap) -> Compiled {
            use ANF::*;
            match a {
                AVar(s) => {
                    let (lbl, wm) = self.get_or_create_varlabel(s, m, p);
                    Compiled {
                        substitutions: p.clone(),
                        dist: self.mgr.var(lbl, true),
                        weight_map: wm,
                        ..Default::default()
                    }
                }
                AVal(Val::Bool(b)) => Compiled {
                    dist: BddPtr::from_bool(b),
                    substitutions: p.clone(),
                    weight_map: m.clone(),
                    ..Default::default()
                },
                And(bl, br) => self.eval_anf_binop(*bl, *br, m, p, &BddManager::and),
                Or(bl, br) => self.eval_anf_binop(*bl, *br, m, p, &BddManager::or),
                Neg(bp) => {
                    let mut p = self.eval_anf(*bp, m, p);
                    p.dist = p.dist.neg();
                    p
                }
            }
        }
        pub fn apply_substitutions(&mut self, bdd: BddPtr, p: &SubstMap) -> BddPtr {
            let mut fin = bdd;
            for (lbl, sub) in p.iter() {
                fin = self.mgr.compose(fin, VarLabel::new(lbl.0), *sub);
            }
            fin
        }

        fn debug_compiled(s: &str, w: &WeightMap, p: &SubstMap, c: &Compiled) {
            let renderw = |ws: &WeightMap| {
                ws.iter()
                    .map(|(k, (l, h))| format!("{k}: ({l}, {h})"))
                    .join(", ")
            };
            let renderp = |ps: &SubstMap| {
                ps.iter()
                    .map(|(k, v)| format!("{k}: {}", v.print_bdd()))
                    .join(", ")
            };
            debug!("{s}, [{}], [{}]", renderw(w), renderp(p));
            debug!("      \\||/  {}", c.dist.print_bdd());
            debug!("      \\||/  {}", c.accept.print_bdd());
            debug!("      \\||/  {}", renderw(&c.weight_map));
            debug!("      \\||/  {}", renderp(&c.substitutions));
            debug!("      \\||/  {}", c.probability.as_f64());
            debug!("      \\||/  {}", c.importance_weight);
            debug!("----------------------------------------");
        }

        pub fn eval_expr(&mut self, e: Expr, m: &WeightMap, p: &SubstMap) -> Compiled {
            use Expr::*;
            match e {
                EAnf(a) => {
                    debug!(">>>anf: {:?}", a);
                    let c = self.eval_anf(*a, m, p);
                    Self::debug_compiled("anf", m, p, &c);
                    c
                }
                // EFst(a) => self.eval_anf(a, m, p),
                // ESnd(a) => self.eval_anf(a, m, p),
                // EProd(al,ar) => self.eval_anf(a, m, p),
                ELetIn(s, ebound, ebody) => {
                    debug!(">>>let-in {}", s);
                    let (lbl, wm) = self.get_or_create_varlabel(s.clone(), m, p);
                    let bound = self.eval_expr(*ebound, &wm, p);
                    let mut bound_substitutions = bound.substitutions.clone();
                    bound_substitutions.insert(UniqueId(lbl.value()), bound.dist);

                    let body = self.eval_expr(*ebody, &bound.weight_map, &bound_substitutions);

                    let mut weight_map = bound.weight_map;
                    weight_map.extend(body.weight_map);

                    let mut substitutions = body.substitutions.clone();
                    // substitutions.extend(body.substitutions);

                    let dist = self.apply_substitutions(body.dist, &substitutions);
                    let accept = self.mgr.and(bound.accept, body.accept);
                    let accept = self.apply_substitutions(accept, &substitutions);

                    let c = Compiled {
                        dist, // : self.mgr.compose(body.dist, lbl, bound.dist),
                        accept,
                        weight_map,
                        substitutions,
                        probability: bound.probability * body.probability,
                        importance_weight: bound.importance_weight * body.importance_weight,
                    };
                    Self::debug_compiled(&format!("let-in {}", s), m, p, &c);
                    c
                }
                EIte(cond, t, f) => {
                    let pred = self.eval_anf(*cond, &m.clone(), &p.clone());
                    let truthy = self.eval_expr(*t, &m.clone(), &p.clone());
                    let falsey = self.eval_expr(*f, &m.clone(), &p.clone());

                    let dist_l = self.mgr.and(pred.dist, truthy.dist);
                    let dist_r = self.mgr.and(pred.dist.neg(), falsey.dist);
                    let dist = self.mgr.or(dist_l, dist_r);

                    let accept_l = self.mgr.and(pred.accept, truthy.accept);
                    let accept_r = self.mgr.and(pred.accept.neg(), falsey.accept);
                    let accept = self.mgr.or(accept_l, accept_r);

                    let mut weight_map = truthy.weight_map.clone();
                    weight_map.extend(falsey.weight_map.clone());
                    let mut substitutions = truthy.substitutions.clone();
                    substitutions.extend(falsey.substitutions.clone());
                    let probability = truthy.probability + falsey.probability;
                    let importance_weight = truthy.convex_combination(&falsey);
                    let c = Compiled {
                        dist,
                        accept,
                        weight_map,
                        substitutions,
                        probability,
                        importance_weight,
                    };
                    Self::debug_compiled("ite", m, p, &c);
                    c
                }
                EFlip(param) => {
                    debug!(">>>flip {param}");
                    let sym = self.fresh();
                    let mut weight_map = m.clone();
                    let lbl = VarLabel::new(sym.0);
                    weight_map.insert(sym, (1.0 - param, param));
                    let c = Compiled {
                        dist: self.mgr.var(lbl, true),
                        accept: BddPtr::PtrTrue,
                        weight_map,
                        substitutions: p.clone(),
                        probability: Probability::new(1.0),
                        importance_weight: 1.0,
                    };
                    Self::debug_compiled("flip {param}", m, p, &c);
                    c
                }
                EObserve(a) => {
                    debug!(">>>observe");

                    let comp = self.eval_anf(*a, m, p);
                    let dist = self.apply_substitutions(comp.dist, p); // FIXME: I don't think there is a need to apply substitutions here, but doing this doesn't hurt
                    let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                    let var_order = VarOrder::linear_order(max_var as usize);
                    let w = dist.wmc(&var_order, &wmc_params);

                    // let accept = self.apply_substitutions(comp.accept, p); // this is always true
                    // let a = self.mgr.and(dist, accept).wmc(&self.var_order, &weight_map_to_params(m));
                    // let z = accept.wmc(&self.var_order, &weight_map_to_params(m));
                    // let w = a / z;

                    let c = Compiled {
                        dist: BddPtr::PtrTrue,
                        accept: dist,
                        weight_map: m.clone(),
                        substitutions: p.clone(),
                        probability: Probability::new(1.0),
                        importance_weight: w as f64,
                    };
                    Self::debug_compiled("observe", m, p, &c);
                    c
                }
                ESample(e) => {
                    debug!(">>>sample");
                    let comp = self.eval_expr(*e, m, p);
                    if comp.accept != BddPtr::PtrTrue {
                        panic!("sample statement includes observe statement");
                    }
                    let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                    let var_order = VarOrder::linear_order(max_var as usize);
                    let theta_q = comp.dist.wmc(&var_order, &wmc_params) as f64;
                    let bern = Bernoulli::new(theta_q).unwrap();
                    let sample = bern.sample(self.rng);
                    let q = Probability::new(if sample { theta_q } else { 1.0 - theta_q });

                    let c = Compiled {
                        dist: BddPtr::from_bool(sample),
                        accept: comp.dist, // FIXME should be `dist` after resolving the initial example
                        weight_map: comp.weight_map.clone(),
                        substitutions: p.clone(),
                        probability: q,
                        importance_weight: 1.0,
                    };
                    Self::debug_compiled("sample", m, p, &c);
                    c
                }
            }
        }
    }
    pub fn compile(env: &mut Env, p: Program) -> Compiled {
        match p {
            Program::Body(e) => env.eval_expr(e, &HashMap::new(), &HashMap::new()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::semantics::*;
    use super::*;
    use grammar::*;
    use inference::*;
    use tracing_test::traced_test;
    use Expr::*;

    fn check_inference(
        i: &str,
        inf: &dyn Fn(&mut Env, Program) -> f32,
        precision: f32,
        s: &str,
        f: f32,
        p: Program,
    ) {
        let mut env_args = EnvArgs::default_args(None);
        let mut env = Env::from_args(&mut env_args);
        let pr = inf(&mut env, p);
        let ret = (f - pr).abs() < precision;
        assert!(
            ret,
            "[check_{i}][{s}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
        );
    }
    fn check_exact(s: &str, f: f32, p: Program) {
        check_inference("exact", &exact_inf, 0.000001, s, f, p);
    }
    fn check_approx(s: &str, f: f32, p: Program, n: usize) {
        check_inference(
            "approx",
            &|env, p| importance_weighting_inf(env, n, p),
            0.01,
            s,
            f,
            p,
        );
    }
    fn check_approx_seeded(s: &str, f: f32, p: Program, n: usize, seeds: &Vec<u64>) {
        // check "perfect seeds" -- vec![1, 7]
        let i = "approx";
        let precision = 0.01;
        let pr = importance_weighting_inf_seeded(seeds.clone(), n, p.clone());
        let ret = (f - pr).abs() < precision;
        assert!(
            ret,
            "[check_{i}][{s}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
        );
    }
    #[test]
    // #[traced_test]
    fn program00() {
        let p00 = lets![
            "x" := val!(true);
            ... var!("x")
        ];
        check_exact("p00", 1.0, Program::Body(p00));
    }
    #[test]
    // #[traced_test]
    fn program01() {
        let p01 = lets![
            "x" := EFlip(1.0/3.0);
            ... var!("x")
        ];
        check_exact("p01", 1.0 / 3.0, Program::Body(p01));
    }
    #[test]
    // #[traced_test]
    fn program02() {
        let mk02 = |ret: Expr| {
            Program::Body(lets![
                "x" := EFlip(1.0/3.0);
                "y" := EFlip(1.0/4.0);
                ... ret
            ])
        };
        check_exact("p02/y  ", 3.0 / 12.0, mk02(var!("y")));
        check_exact("p02/x  ", 4.0 / 12.0, mk02(var!("x")));
        check_exact("p02/x|y", 6.0 / 12.0, mk02(anf!(or!("x", "y"))));
        check_exact("p02/x&y", 1.0 / 12.0, mk02(anf!(and!("x", "y"))));
    }

    #[test]
    // #[traced_test]
    fn program03() {
        let mk03 = |ret: Expr| {
            Program::Body(lets![
                "x" := EFlip(1.0/3.0);
                "y" := EFlip(1.0/4.0);
                "_" := EObserve(Box::new(or!("x", "y")));
                ... ret
            ])
        };
        check_exact("p03/y  ", 3.0 / 6.0, mk03(var!("y")));
        check_exact("p03/x  ", 4.0 / 6.0, mk03(var!("x")));
        check_exact("p03/x|y", 6.0 / 6.0, mk03(anf!(or!("x", "y"))));
        check_exact("p03/x&y", 1.0 / 6.0, mk03(anf!(and!("x", "y"))));
    }

    #[test]
    #[traced_test]
    fn program04_seeded() {
        let mk04 = |ret: Expr| {
            Program::Body(lets![
                "x" := ESample(Box::new(EFlip(1.0/3.0)));
                "y" := EFlip(1.0/4.0);
                "_" := EObserve(Box::new(or!("x", "y")));
                ... ret
            ])
        };
        // let perfect_seeds = vec![1, 7];
        let s = vec![1, 7];
        let n = 2;
        check_approx_seeded("p04s/y  ", 3.0 / 6.0, mk04(var!("y")), n, &s);
        check_approx_seeded("p04s/x  ", 4.0 / 6.0, mk04(var!("x")), n, &s);
        check_approx_seeded("p04s/x|y", 6.0 / 6.0, mk04(anf!(or!("x", "y"))), n, &s);
        check_approx_seeded("p04s/x&y", 1.0 / 6.0, mk04(anf!(and!("x", "y"))), n, &s);
    }

    #[test]
    #[ignore]
    // #[traced_test]
    fn program04_approx() {
        let mk04 = |ret: Expr| {
            Program::Body(lets![
                "x" := ESample(Box::new(EFlip(1.0/3.0)));
                "y" := EFlip(1.0/4.0);
                "_" := EObserve(Box::new(or!("x", "y")));
                ... ret
            ])
        };
        check_approx("p04/y  ", 3.0 / 6.0, mk04(var!("y")), 10000);
        check_approx("p04/x  ", 4.0 / 6.0, mk04(var!("x")), 10000);
        check_approx("p04/x|y", 6.0 / 6.0, mk04(anf!(or!("x", "y"))), 10000);
        check_approx("p04/x&y", 1.0 / 6.0, mk04(anf!(and!("x", "y"))), 10000);
    }

    #[test]
    #[ignore]
    // #[traced_test]
    fn shared_free_variable() {
        let p = Program::Body(lets![
           "x" := flip!(1/3);
           "l" := sample!(var!("x"));
           "r" := sample!(var!("x"));
           ... anf!(and!("x", "y"))
        ]);
    }
}
