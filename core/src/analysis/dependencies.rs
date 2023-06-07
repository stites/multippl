use crate::annotate::grammar::*;
use crate::grammar::*;

use itertools::*;
use std::collections::hash_map;
use std::collections::{HashMap, HashSet};
use std::vec;
use tracing::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Dep {
    Var(NamedVar),
    Sample(NamedVar),
}
impl Dep {
    pub fn named_var(&self) -> NamedVar {
        match self {
            Self::Var(x) => x.clone(),
            Self::Sample(x) => x.clone(),
        }
    }
    pub fn map<X>(&self, f: &dyn Fn(NamedVar) -> X) -> X {
        match self {
            Self::Var(x) => f(x.clone()),
            Self::Sample(x) => f(x.clone()),
        }
    }
    pub fn var(&self) -> Option<NamedVar> {
        match self {
            Self::Var(x) => Some(x.clone()),
            Self::Sample(x) => None,
        }
    }
    pub fn sample(&self) -> Option<NamedVar> {
        match self {
            Self::Var(x) => None,
            Self::Sample(x) => Some(x.clone()),
        }
    }
    pub fn as_dep(names: HashSet<NamedVar>, constructor: &dyn Fn(NamedVar) -> Dep) -> HashSet<Dep> {
        names.into_iter().map(constructor).collect()
    }
    pub fn as_vars(names: HashSet<NamedVar>) -> HashSet<Dep> {
        Self::as_dep(names, &Dep::Var)
    }
    pub fn vars(deps: &HashSet<Dep>) -> HashSet<NamedVar> {
        deps.iter().map(|x| x.var()).flatten().collect()
    }
}
#[derive(Default, Clone)]
pub struct DependenceMap(pub HashMap<NamedVar, HashSet<Dep>>);
impl DependenceMap {
    pub fn insert(&mut self, var: NamedVar, deps: HashSet<Dep>) {
        self.0.insert(var, deps);
    }
    pub fn get(&self, var: &NamedVar) -> Option<&HashSet<Dep>> {
        self.0.get(var)
    }
    pub fn unsafe_get(&self, var: &NamedVar) -> &HashSet<Dep> {
        self.get(var)
            .unwrap_or_else(|| panic!("{:?} not found in {:?}", var, self.0.keys()))
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn iter<'a>(&'a self) -> hash_map::Iter<'a, NamedVar, HashSet<Dep>> {
        self.0.iter()
    }
    pub fn family_iter<'a>(&'a self) -> vec::IntoIter<HashSet<Dep>> {
        self.0
            .iter()
            .map(|(v, ps)| {
                let mut ps = ps.clone();
                ps.insert(Dep::Var(v.clone()));
                ps
            })
            .collect_vec()
            .into_iter()
    }
}
pub struct DependencyEnv {
    map: DependenceMap,
}
impl DependencyEnv {
    pub fn new() -> Self {
        Self {
            map: Default::default(),
        }
    }
    fn scan_anf(&self, a: &AnfAnn) -> HashSet<NamedVar> {
        use Anf::*;
        match a {
            AVar(nv, s) => HashSet::from([nv.clone()]),
            AVal((), _) => HashSet::new(),
            And(bl, br) => {
                let mut vs = self.scan_anf(bl);
                vs.extend(self.scan_anf(br));
                vs
            }
            Or(bl, br) => {
                let mut vs = self.scan_anf(bl);
                vs.extend(self.scan_anf(br));
                vs
            }
            Neg(bp) => self.scan_anf(bp),
        }
    }
    fn scan_expr(&mut self, e: &EExprAnn) -> HashSet<Dep> {
        use crate::grammar::EExpr::*;
        match e {
            EAnf((), a) => Dep::as_vars(self.scan_anf(a)),
            // FIXME : definitely not desiarable, fix later
            // FIXME vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
            EPrj((), i, a) => Dep::as_vars(self.scan_anf(a)),
            EProd((), az) => Dep::as_vars(
                az.iter()
                    .map(|a| self.scan_anf(a).into_iter())
                    .flatten()
                    .collect(),
            ),
            // FIXME ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            ELetIn(nv, s, ebound, ebody) => {
                let deps = self.scan_expr(ebound);
                self.map.insert(nv.clone(), deps);
                self.scan_expr(ebody)
            }
            EIte(v, cond, t, f) => {
                let mut deps = Dep::as_vars(self.scan_anf(cond));
                deps.extend(self.scan_expr(t));
                deps.extend(self.scan_expr(f));
                deps
            }
            ESample((), e) => self
                .scan_expr(e)
                .into_iter()
                .map(&|x: Dep| x.map(&Dep::Sample))
                .collect(),
            EObserve((), a) => Dep::as_vars(self.scan_anf(a)),
            EFlip(_, p) => HashSet::new(),
        }
    }

    pub fn scan(&mut self, p: &ProgramAnn) -> DependenceMap {
        match p {
            Program::Body(b) => {
                self.scan_expr(b);
                self.map.clone()
            }
        }
    }
}

#[cfg(test)]
#[allow(unused_mut)] // for optional patterns in test macros
#[allow(unused_assignments)]
mod tests {
    use super::*;
    use crate::annotate::grammar::named;
    use crate::compile::*;
    use crate::grammar::*;
    use crate::grammar_macros::*;
    use crate::typecheck::grammar::{EExprTyped, ProgramTyped};
    use crate::typecheck::typecheck;
    use crate::*;
    use tracing::*;
    use tracing_test::traced_test;

    macro_rules! assert_root {
        ($deps:ident : $xvar:expr $(, $var:expr)* $(,)?) => {{
            let ds = $deps.unsafe_get(&$xvar);
            assert_eq!(ds, &HashSet::new(), "{:?} deps should be empty", $xvar);

            $(
            let ds = $deps.unsafe_get(&$var);
            assert_eq!(ds, &HashSet::new(), "{:?} deps should be empty", $var);
            )*
        }}
    }
    macro_rules! assert_family {
        ($deps:ident : $xvar:expr => $f0:expr $(; sample = $sample0:literal)? $(, $var:expr  $(; sample = $sample:literal)?)* $(,)?) => {{
            let ds = $deps.unsafe_get(&$xvar);
            let mut fam = HashSet::new();
            let mut sampled0 = false;
            $(
                sampled0 = $sample0;
            )?
            let var0 = if sampled0 { Dep::Sample($f0.clone()) }  else {  Dep::Var($f0.clone()) };


            fam.insert(var0);
            $(
            let mut sampled = false;
            $(
                sampled = $sample;
            )?
            let var = if sampled { Dep::Sample($var.clone()) }  else {  Dep::Var($var.clone()) };
                println!("var: {:?}, sampled? {:?}", var, sampled);
            fam.insert(var);
            )*
            assert_eq!(ds, &fam, "var {:?}: expected {:?}, found: {:?}", $xvar, ds, fam);
        }}
    }

    #[test]
    pub fn test_dependencies_for_simple_program() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
           ...? b!("x")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        assert_root!(deps: named(0, "x"));
        assert_eq!(deps.len(), 1);
    }

    #[test]
    pub fn test_dependencies_with_boolean_operator() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= b!("x" && "y");
            "q" ;= flip!(1/3);
            "w" ;= b!("q" && "z");
           ...? b!("z")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        let xvar = named(0, "x");
        let yvar = named(2, "y");
        let zvar = named(4, "z");
        let qvar = named(5, "q");
        let wvar = named(7, "w");
        assert_root!(deps: xvar, yvar, qvar);
        assert_family!(deps: zvar => xvar, yvar);
        assert_family!(deps: wvar => qvar, zvar);
        assert_eq!(deps.len(), 5);
    }

    #[test]
    pub fn test_dependencies_captures_tuples() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "t" ;= b!("x", "y");
            "f" ;= fst!("t");
           ...? b!("t")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        let xvar = named(0, "x");
        let yvar = named(2, "y");
        let tvar = named(4, "t");
        let fvar = named(5, "f");
        assert_root!(deps: xvar, yvar);
        assert_family!(deps: tvar => xvar, yvar);
        assert_family!(deps: fvar => tvar);
        assert_eq!(deps.len(), 4);
    }

    #[test]
    pub fn test_dependencies_works_as_expected_for_samples() {
        let p = program!(lets![
           "x" ;= flip!(1/3);
           "s" ;= sample!(var!("x"));
           ...? b!("s")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        let xvar = named(0, "x");
        let svar = named(2, "s");
        assert_root!(deps: xvar);
        assert_family!(deps: svar => xvar; sample = true);
        assert_eq!(deps.len(), 2);
    }

    #[test]
    pub fn test_dependencies_ite_sample() {
        let p = program!(lets![
            "x" ;= flip!(1/5);
            "y" ;= flip!(1/5);
            "z" ;= flip!(1/5);
            "i" ;= ite!(
                if ( var!("x") )
                then { sample!(var!("y")) }
                else { var!("z") });
            ...? b!("i")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        let xvar = named(0, "x");
        let yvar = named(2, "y");
        let zvar = named(4, "z");
        let ivar = named(6, "i");
        assert_root!(deps: xvar, yvar, zvar);
        assert_family!(deps: ivar => xvar, yvar; sample = true, zvar);
        assert_eq!(deps.len(), 4);
    }
}
