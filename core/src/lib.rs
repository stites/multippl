#![allow(dead_code)]
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
use std::collections::HashMap;
use std::string::String;
use tracing::{debug, error, info, span, warn, Level};

pub mod grammar {
    #[derive(Debug, Copy, Clone)]
    pub enum Ty {
        Bool,
        // Prod(Box<Ty>, Box<Ty>), // TODO for now, no tuples
    }
    #[derive(Debug, Copy, Clone)]
    pub enum Val {
        Bool(bool),
        // Prod(Box<Val>, Box<Val>), // TODO punt
    }
    #[derive(Debug, Clone)]
    pub enum ANF {
        AVar(String),
        AVal(Val),
        // TODO: not sure this is where I should add booleans, but it makes
        // the observe statements stay closer to the semantics: ~observe anf~
        And(Box<ANF>, Box<ANF>),
        Or(Box<ANF>, Box<ANF>),
        Neg(Box<ANF>),
    }
    #[derive(Debug, Clone)]
    pub enum Expr {
        EAnf(Box<ANF>),
        // TODO Ignore product types for now:
        // EFst (Box<ANF>),
        // ESnd (Box<ANF>),
        // EProd (Box<ANF>, Box<ANF>),

        // TODO Ignore function calls for now
        // EApp(String, Box<ANF>),
        ELetIn(String, Box<Expr>, Box<Expr>),
        EIte(Box<ANF>, Box<Expr>, Box<Expr>),
        EFlip(f32),
        EObserve(Box<ANF>),
        ESample(Box<Expr>),
    }
    // TODO
    // structure Func where
    //   name : String
    //   arg : String × Ty
    //   ret : Ty
    //   body : Expr
    // deriving Repr
    impl Expr {
        fn strip_samples(&self) -> Expr {
            use Expr::*;
            match self {
                ESample(e) => *e.clone(),
                ELetIn(s, x, y) => ELetIn(
                    s.clone(),
                    Box::new(x.strip_samples()),
                    Box::new(y.strip_samples()),
                ),
                EIte(p, x, y) => EIte(
                    p.clone(),
                    Box::new(x.strip_samples()),
                    Box::new(y.strip_samples()),
                ),
                e => e.clone(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Program {
        Body(Expr),
        // TODO
        // | define (f: Func) (rest: Program) : Program
    }
    impl Program {
        fn strip_samples(&self) -> Program {
            use Program::*;
            match self {
                Body(e) => Body(e.strip_samples()),
            }
        }
    }
}
pub mod semantics {
    use super::*;
    use grammar::*;
    use itertools::*;
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
    fn const_weight() -> (f32, f32) {
        (0.5, 0.5)
    }
    fn weight_map_to_params(m: &WeightMap) -> (WmcParams<f32>, u64) {
        let mut wmc_params = WmcParams::new(0.0, 1.0);
        let mut max = 0;
        for (lbl, (l, h)) in m {
            wmc_params.set_weight(VarLabel::new(lbl.0), *l, *h);
            if lbl.0 > max {
                max = lbl.0;
            }
        }
        (wmc_params, max)
    }
    type SubstMap = HashMap<UniqueId, BddPtr>;

    #[derive(Debug, Clone)]
    pub struct Compiled {
        dist: BddPtr,
        accept: BddPtr,
        weight_map: WeightMap, // must be a hashmap as sample will collapse variables
        substitutions: SubstMap,
        probability: Probability,
        importance_weight: f64,
    }
    impl Compiled {
        fn convex_combination(&self, o: &Compiled) -> f64 {
            self.probability.as_f64() * self.importance_weight
                + o.probability.as_f64() * o.importance_weight
        }

        // FIXME: need to think about these semantics again.
        fn conj_extend<T: LruTable<BddPtr>>(&mut self, mgr: &mut BddManager<T>, o: Compiled) {
            self.dist = mgr.and(self.dist, o.dist);
            self.accept = mgr.and(self.accept, o.accept);
            self.weight_map.extend(o.weight_map);
            self.substitutions.extend(o.substitutions);
            self.probability = self.probability * o.probability;
            self.importance_weight *= o.importance_weight;
        }
        // FIXME: need to think about these semantics again.
        fn disj_extend<T: LruTable<BddPtr>>(&mut self, mgr: &mut BddManager<T>, o: Compiled) {
            self.dist = mgr.or(self.dist, o.dist);
            self.accept = mgr.or(self.accept, o.accept);
            self.weight_map.extend(o.weight_map.clone());
            self.substitutions.extend(o.substitutions.clone());
            self.probability = self.probability + o.probability;
            self.importance_weight = self.convex_combination(&o);
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
        names: HashMap<String, UniqueId>,
        gensym: u64,
        mgr: BddManager<AllTable<BddPtr>>,
        rng: StdRng,
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
        names: HashMap<String, UniqueId>,
        gensym: u64,
        mgr: &'a mut BddManager<AllTable<BddPtr>>,
        rng: &'a mut StdRng,
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
        fn get_or_create_varlabel(&mut self, s: String, m: &WeightMap) -> (VarLabel, WeightMap) {
            match self.get_var(s) {
                None => {
                    let id = self.fresh();
                    let lbl = VarLabel::new(id.0);
                    let mut mm = m.clone();
                    mm.insert(id, const_weight());
                    (lbl, mm)
                }
                Some(sym) => {
                    let lbl = VarLabel::new(sym.0);
                    (lbl, m.clone())
                }
            }
        }
        pub fn eval_anf(&mut self, a: ANF, m: &WeightMap, p: &SubstMap) -> Compiled {
            use ANF::*;
            match a {
                AVar(s) => {
                    let (lbl, wm) = self.get_or_create_varlabel(s, m);
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
                And(bl, br) => {
                    let mut l = self.eval_anf(*bl, m, p);
                    let r = self.eval_anf(*br, m, p);
                    l.conj_extend(self.mgr, r);
                    l
                }
                Or(bl, br) => {
                    let mut l = self.eval_anf(*bl, m, p);
                    let r = self.eval_anf(*br, m, p);
                    l.disj_extend(self.mgr, r);
                    l
                }
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

        /// Print a debug form of the BDD with the label remapping given by `map`
        pub fn print_bdd_lbl(ptr: BddPtr, map: &HashMap<VarLabel, VarLabel>) -> String {
            match ptr {
                BddPtr::PtrTrue => String::from("T"),
                // BddPtr::PtrFalse => String::from("T"),
                BddPtr::PtrFalse => String::from("F"), // TODO: check that this is right?
                BddPtr::Compl(n) => {
                    let s = Self::print_bdd_lbl(BddPtr::Reg(n), map);
                    format!("!{}", s)
                }
                BddPtr::Reg(n) => unsafe {
                    let l_p = (*n).low;
                    let r_p = (*n).high;
                    let l_s = Self::print_bdd_lbl(l_p, map);
                    let r_s = Self::print_bdd_lbl(r_p, map);
                    let lbl = (*n).var;
                    format!(
                        "({:?}, {}, {})",
                        map.get(&lbl).unwrap_or(&lbl).value(),
                        l_s,
                        r_s
                    )
                },
            }
        }

        pub fn print_bdd(ptr: BddPtr) -> String {
            Self::print_bdd_lbl(ptr, &HashMap::new())
        }

        fn debug_compiled(s: &str, w: &WeightMap, p: &SubstMap, c: Compiled) {
            debug!("[evalExpr][{s}]");
            let wmap = w
                .iter()
                .map(|(k, (l, h))| format!("{k}: ({l}, {h})"))
                .join(", ");
            let subm = p
                .iter()
                .map(|(k, v)| format!("{k}: {}", Self::print_bdd(v.clone())))
                .join(", ");
            debug!("[evalExpr] _,  [{}], [{}]", wmap, subm);
            debug!("[evalExpr]      \\||/  {}", Self::print_bdd(c.dist));
            debug!("[evalExpr]      \\||/  {}", Self::print_bdd(c.accept));
            let wmap = c
                .weight_map
                .iter()
                .map(|(k, (l, h))| format!("{k}: ({l}, {h})"))
                .join(", ");
            let subm = c
                .substitutions
                .iter()
                .map(|(k, v)| format!("{k}: {}", Self::print_bdd(v.clone())))
                .join(", ");
            debug!("[evalExpr]      \\||/  {}", wmap);
            debug!("[evalExpr]      \\||/  {}", subm);
            debug!("[evalExpr]      \\||/  {}", c.probability.as_f64());
            debug!("[evalExpr]      \\||/  {}", c.importance_weight);
            debug!("[evalExpr][{s}]");
        }

        pub fn eval_expr(&mut self, e: Expr, m: &WeightMap, p: &SubstMap) -> Compiled {
            use Expr::*;
            match e {
                EAnf(a) => self.eval_anf(*a, m, p),
                // EFst(a) => self.eval_anf(a, m, p),
                // ESnd(a) => self.eval_anf(a, m, p),
                // EProd(al,ar) => self.eval_anf(a, m, p),
                ELetIn(s, ebound, ebody) => {
                    let (lbl, wm) = self.get_or_create_varlabel(s, m);
                    let bound = self.eval_expr(*ebound, &wm, p);
                    let mut subst = bound.substitutions.clone();
                    subst.insert(UniqueId(lbl.value()), bound.dist);
                    let body = self.eval_expr(*ebody, &bound.weight_map, &subst);
                    let mut weight_map = bound.weight_map;
                    weight_map.extend(body.weight_map);
                    let mut substitutions = bound.substitutions.clone();
                    substitutions.extend(body.substitutions);
                    let body_accept = self.mgr.compose(body.accept, lbl, bound.dist);
                    Compiled {
                        dist: self.mgr.compose(body.dist, lbl, bound.dist),
                        accept: self.mgr.and(bound.accept, body_accept),
                        weight_map,
                        substitutions,
                        probability: bound.probability * body.probability,
                        importance_weight: bound.importance_weight * body.importance_weight,
                    }
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
                    Compiled {
                        dist,
                        accept,
                        weight_map,
                        substitutions,
                        probability,
                        importance_weight,
                    }
                }
                EFlip(param) => {
                    let sym = self.fresh();
                    let mut weight_map = m.clone();
                    let lbl = VarLabel::new(sym.0);
                    weight_map.insert(sym, (1.0 - param, param));
                    Compiled {
                        dist: self.mgr.var(lbl, true),
                        accept: BddPtr::PtrTrue,
                        weight_map,
                        substitutions: p.clone(),
                        probability: Probability::new(1.0),
                        importance_weight: 1.0,
                    }
                }
                EObserve(a) => {
                    let comp = self.eval_anf(*a, m, p);
                    let dist = self.apply_substitutions(comp.dist, p); // FIXME: I don't think there is a need to apply substitutions here, but doing this doesn't hurt
                    let (wmc_params, max_var) = weight_map_to_params(&comp.weight_map);
                    let var_order = VarOrder::linear_order(max_var as usize);
                    let w = dist.wmc(&var_order, &wmc_params);

                    // let accept = self.apply_substitutions(comp.accept, p); // this is always true
                    // let a = self.mgr.and(dist, accept).wmc(&self.var_order, &weight_map_to_params(m));
                    // let z = accept.wmc(&self.var_order, &weight_map_to_params(m));
                    // let w = a / z;

                    Compiled {
                        dist: BddPtr::PtrTrue,
                        accept: dist,
                        weight_map: m.clone(),
                        substitutions: p.clone(),
                        probability: Probability::new(1.0),
                        importance_weight: w as f64,
                    }
                }
                ESample(e) => {
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
                    Compiled {
                        dist: BddPtr::from_bool(sample),
                        accept: BddPtr::PtrTrue, // FIXME should be `dist` after resolving the initial example
                        weight_map: m.clone(),
                        substitutions: p.clone(),
                        probability: q,
                        importance_weight: 1.0,
                    }
                }
            }
        }
    }
    pub fn compile(env: &mut Env, p: Program) -> Compiled {
        match p {
            Program::Body(e) => env.eval_expr(e, &HashMap::new(), &HashMap::new()),
        }
    }
    pub fn wmc_prob(env: &mut Env, c: Compiled) -> (f32, f32) {
        let (params, mx) = weight_map_to_params(&c.weight_map);
        let var_order = VarOrder::linear_order(mx as usize);
        let a = env.mgr.and(c.dist, c.accept).wmc(&var_order, &params);
        let z = c.accept.wmc(&var_order, &params);
        (a, z)
    }
    pub fn exact_inf(env: &mut Env, p: Program) -> f32 {
        let c = compile(env, p);
        let (a, z) = wmc_prob(env, c);
        a / z
    }
}

#[cfg(test)]
mod tests {
    use super::grammar::*;
    use super::semantics::*;
    use super::*;
    use Expr::*;

    #[macro_export]
    macro_rules! val {
        ( $x:ident ) => {
            Expr::EAnf(Box::new(ANF::AVal(Val::Bool($x))))
        };
    }
    #[macro_export]
    macro_rules! var {
        ( $x:literal ) => {
            Expr::EAnf(Box::new(ANF::AVar($x.to_string())))
        };
    }
    #[macro_export]
    macro_rules! or {
        ( $x:literal, $y:literal ) => {{
            ANF::Or(
                Box::new(ANF::AVar($x.to_string())),
                Box::new(ANF::AVar($y.to_string())),
            )
        }};
    }
    #[macro_export]
    macro_rules! and {
        ( $x:literal, $y:literal ) => {{
            ANF::And(
                Box::new(ANF::AVar($x.to_string())),
                Box::new(ANF::AVar($y.to_string())),
            )
        }};
    }
    #[macro_export]
    macro_rules! anf {
        ( $x:expr ) => {{
            Expr::EAnf(Box::new($x))
        }};
    }
    #[macro_export]
    macro_rules! lets {
        ( $( $var:literal := $bound:expr );+ ;... $body:expr ) => {
            {
                let mut fin = Box::new($body);
                $(
                    fin = Box::new(Expr::ELetIn($var.to_string(), Box::new($bound), fin));
                )+
                *fin
            }
        };
    }

    fn check_exact(s: &str, f: f32, env: &mut Env, p: Program) {
        let pr = exact_inf(env, p);
        // print!("[check_exact][{s}]");
        let ret = (pr - f).abs() < 0.000001;
        assert!(ret, "[check_exact][{s}][err][exp: {f}] != {pr}");
        // println!(if ret {
        //     format!("[ok!][exp: {f}] == {pr}")
        // } else {
        //     format!("[err][exp: {f}] != {pr}")
        // });
    }

    #[test]
    fn program00() {
        let mut env_args = EnvArgs::default_args(None);
        let mut env = Env::from_args(&mut env_args);
        let p00 = lets![
            "x" := val!(true);
            ... var!("x")
        ];
        check_exact("p00", 1.0, &mut env, Program::Body(p00));
    }
    #[test]
    fn program01() {
        let mut env_args = EnvArgs::default_args(None);
        let mut env = Env::from_args(&mut env_args);

        let p01 = lets![
            "x" := EFlip(1.0/3.0);
            ... var!("x")
        ];
        check_exact("p01", 1.0 / 3.0, &mut env, Program::Body(p01));
    }
    #[test]
    fn program02() {
        let mut env_args = EnvArgs::default_args(None);
        let mut env = Env::from_args(&mut env_args);

        let mk02 = |ret| {
            lets![
                "x" := EFlip(0.3333);
                "y" := EFlip(1.0/4.0);
                ... ret
            ]
        };
        check_exact(
            "p02/y  ",
            3.0 / 12.0,
            &mut env,
            Program::Body(mk02(var!("y"))),
        );
        check_exact(
            "p02/x  ",
            4.0 / 12.0,
            &mut env,
            Program::Body(mk02(var!("x"))),
        );
        check_exact(
            "p02/x|y",
            1.0 / 12.0,
            &mut env,
            Program::Body(mk02(anf!(or!("x", "y")))),
        );
        check_exact(
            "p02/x&y",
            6.0 / 12.0,
            &mut env,
            Program::Body(mk02(anf!(and!("x", "y")))),
        );
    }

    #[test]
    fn program03() {
        let mut env_args = EnvArgs::default_args(None);
        let mut env = Env::from_args(&mut env_args);

        let mk03 = |ret| {
            lets![
                "x" := EFlip(0.3333);
                "y" := EFlip(1.0/4.0);
                "_" := EObserve(Box::new(or!("x", "y")));
                ... ret
            ]
        };
        check_exact(
            "p03/y  ",
            3.0 / 6.0,
            &mut env,
            Program::Body(mk03(var!("y"))),
        );
        check_exact(
            "p03/x  ",
            4.0 / 6.0,
            &mut env,
            Program::Body(mk03(var!("x"))),
        );
        check_exact(
            "p03/x|y",
            1.0 / 6.0,
            &mut env,
            Program::Body(mk03(anf!(or!("x", "y")))),
        );
        check_exact(
            "p03/x&y",
            6.0 / 6.0,
            &mut env,
            Program::Body(mk03(anf!(and!("x", "y")))),
        );
    }
}
