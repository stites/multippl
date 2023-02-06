#![allow(dead_code)]
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
use std::collections::HashMap;
use std::string::String;

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

    #[derive(Debug, Clone)]
    pub enum Program {
        Body(Expr),
        // TODO
        // | define (f: Func) (rest: Program) : Program
    }
}
pub mod semantics {
    use super::*;
    use grammar::*;
    use rand::distributions::{Bernoulli, Distribution};
    use rand::rngs::StdRng;
    use rsdd::builder::cache::*;
    use rsdd::repr::bdd::*;
    use rsdd::repr::ddnnf::*;
    use rsdd::repr::var_label::*;
    use rsdd::repr::var_order::VarOrder;
    use rsdd::repr::wmc::WmcParams;
    use rsdd::sample::probability::Probability;

    type WeightMap = HashMap<VarLabel, (f32, f32)>;
    fn const_weight() -> (f32, f32) {
        (0.5, 0.5)
    }
    fn weight_map_to_params(m: &WeightMap) -> WmcParams<f32> {
        let mut wmc_params = WmcParams::new(0.0, 1.0);
        for (lbl, (l, h)) in m {
            wmc_params.set_weight(*lbl, *l, *h);
        }
        wmc_params
    }
    type SubstMap = HashMap<VarLabel, BddPtr>;

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

    pub type Sym = u64;

    pub struct Env<'a> {
        vars: HashMap<String, Sym>,
        gensym: Sym,
        var_order: VarOrder,
        mgr: &'a mut BddManager<AllTable<BddPtr>>,
        rng: &'a mut StdRng,
    }
    impl<'a> Env<'a> {
        fn _fresh(&mut self, ovar: Option<String>) -> Sym {
            let sym = self.gensym;
            self.gensym += 1;
            let var = ovar.unwrap_or(format!("_{sym}"));
            self.vars.insert(var, sym);
            sym
        }
        fn fresh(&mut self) -> Sym {
            self._fresh(None)
        }
        fn get_var(&self, var: String) -> Option<Sym> {
            self.vars.get(&var).copied()
        }
        fn get_or_create(&mut self, var: String) -> Sym {
            let osym = self.get_var(var.clone());
            osym.unwrap_or_else(|| self._fresh(Some(var)))
        }
        fn get_or_create_varlabel(&mut self, s: String, m: &WeightMap) -> (VarLabel, WeightMap) {
            match self.get_var(s) {
                None => {
                    let lbl = VarLabel::new(self.fresh());
                    let mut mm = m.clone();
                    mm.insert(lbl, const_weight());
                    (lbl, mm)
                }
                Some(sym) => {
                    let lbl = VarLabel::new(sym);
                    (lbl, m.clone())
                }
            }
        }
        fn eval_anf(&mut self, a: ANF, m: &WeightMap, p: &SubstMap) -> Compiled {
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
        fn apply_substitutions(&mut self, bdd: BddPtr, p: &SubstMap) -> BddPtr {
            let mut fin = bdd;
            for (lbl, sub) in p.iter() {
                fin = self.mgr.compose(fin, *lbl, *sub);
            }
            fin
        }
        fn eval_expr(&mut self, e: Expr, m: &WeightMap, p: &SubstMap) -> Compiled {
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
                    subst.insert(lbl, bound.dist);
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
                    let lbl = VarLabel::new(sym);
                    weight_map.insert(lbl, (1.0 - param, param));
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
                    let w = dist.wmc(&self.var_order, &weight_map_to_params(m));

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
                    let theta_q = comp
                        .dist
                        .wmc(&self.var_order, &weight_map_to_params(&comp.weight_map))
                        as f64;
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
}

#[cfg(test)]
mod tests {
    use super::grammar::*;
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

    #[test]
    fn program00() {
        let p00 = ELetIn(String::from("x"), Box::new(val!(true)), Box::new(var!("x")));
        let p01 = lets![
            "x" := EFlip(1.0/3.0);
            ... var!("x")
        ];
        let p02 = lets![
            "x" := EFlip(0.3333);
            "y" := EFlip(1.0/4.0);
            ... anf!(or!("x", "y"))
        ];
        let p03 = lets![
            "x" := EFlip(0.3333);
            "y" := EFlip(1.0/4.0);
            "_" := EObserve(Box::new(or!("x", "y")));
            ... var!("x")
        ];
        let p1 = lets![
            "x" := ESample(Box::new(EFlip(0.3333)));
            "y" := EFlip(1.0/4.0);
            "_" := EObserve(Box::new(or!("x", "y")));
            ... var!("x")
        ];
    }
}
