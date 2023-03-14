use crate::annotate::grammar::*;
use crate::compile::CompileError;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::CompileError::Generic;
use grammar::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use std::collections::{HashMap, HashSet};
use tracing::*;

pub mod grammar {
    use super::*;
    pub use crate::annotate::grammar::{BddVar, NamedVar, Var};
    pub use crate::uniquify::grammar::UniqueId;
    use std::fmt;
    use std::fmt::*;

    #[derive(Clone, Eq, PartialEq, Debug)]
    pub struct Decorated<X> {
        pub var: X,
        pub above: HashSet<Var>,
        pub below: HashSet<Var>,
    }
    pub type DecoratedNamedVar = Decorated<NamedVar>;
    pub type DecoratedBddVar = Decorated<BddVar>;

    #[derive(Clone, Eq, PartialEq, Debug)]
    pub enum DecoratedVar {
        Bdd(DecoratedBddVar),
        Named(DecoratedNamedVar),
    }
    impl DecoratedVar {
        pub fn new(v: &Var, above: HashSet<Var>, below: HashSet<Var>) -> Self {
            match v {
                Var::Named(var) => DecoratedVar::Named(Decorated {
                    above,
                    below,
                    var: var.clone(),
                }),
                Var::Bdd(var) => DecoratedVar::Bdd(Decorated {
                    above,
                    below,
                    var: var.clone(),
                }),
            }
        }
        pub fn id(&self) -> UniqueId {
            self.var().id()
        }
        pub fn below(&self) -> &HashSet<Var> {
            match self {
                DecoratedVar::Named(d) => &d.below,
                DecoratedVar::Bdd(d) => &d.below,
            }
        }
        pub fn above(&self) -> &HashSet<Var> {
            match self {
                DecoratedVar::Named(d) => &d.above,
                DecoratedVar::Bdd(d) => &d.above,
            }
        }
        pub fn var(&self) -> Var {
            match self {
                DecoratedVar::Named(d) => Var::Named(d.var.clone()),
                DecoratedVar::Bdd(d) => Var::Bdd(d.var.clone()),
            }
        }

        pub fn from_var(var: &Var) -> DecoratedVar {
            DecoratedVar::new(var, HashSet::new(), HashSet::new())
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Analysis;

    impl ξ<Analysis> for AVarExt {
        type Ext = DecoratedVar;
    }
    impl ξ<Analysis> for AValExt {
        type Ext = ();
    }
    pub type AnfAnlys = Anf<Analysis>;

    impl ξ<Analysis> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Analysis> for EFstExt {
        type Ext = ();
    }
    impl ξ<Analysis> for ESndExt {
        type Ext = ();
    }
    impl ξ<Analysis> for EPrjExt {
        type Ext = ();
    }
    impl ξ<Analysis> for EProdExt {
        type Ext = ();
    }
    impl ξ<Analysis> for ELetInExt {
        type Ext = DecoratedVar;
    }
    impl ξ<Analysis> for EIteExt {
        type Ext = ();
    }
    impl ξ<Analysis> for EFlipExt {
        type Ext = DecoratedVar;
    }
    impl ξ<Analysis> for EObserveExt {
        type Ext = ();
    }
    impl ξ<Analysis> for ESampleExt {
        type Ext = ();
    }

    pub type ExprAnlys = Expr<Analysis>;
    pub type ProgramAnlys = Program<Analysis>;
}

pub struct AnalysisEnv {
    seen: HashSet<Var>,
    above_below: HashMap<Var, (HashSet<Var>, HashSet<Var>)>,
    decor: HashMap<Var, DecoratedVar>,
}

impl AnalysisEnv {
    /// ids from annotate pipeline
    pub fn new(ids: &HashMap<UniqueId, Var>) -> AnalysisEnv {
        let decor: HashMap<Var, DecoratedVar> = ids
            .iter()
            .map(|(_, v)| (v.clone(), DecoratedVar::from_var(v)))
            .collect();
        AnalysisEnv {
            seen: HashSet::new(),
            above_below: HashMap::new(),
            decor,
        }
    }

    pub fn insert_below(&mut self, v: &Var) {
        for (k, (a, b)) in self.above_below.iter_mut() {
            if k != v {
                b.insert(v.clone());
            }
        }
    }

    pub fn insert_above(&mut self, v: &Var) {
        let above: HashSet<Var> = self.seen.iter().filter(|x| x != &v).cloned().collect();
        match self.above_below.get(&v.clone()) {
            Some((a, b)) => {
                self.above_below.insert(v.clone(), (above, b.clone()));
            }
            None => {
                self.above_below.insert(v.clone(), (above, HashSet::new()));
            }
        }
        self.seen.insert(v.clone());
        let (a, b) = self.above_below.get(&v.clone()).unwrap();
        let above: HashSet<String> = a.iter().cloned().map(|v| v.debug_id()).collect();

        let below: HashSet<String> = b.iter().cloned().map(|v| v.debug_id()).collect();

        debug!("[{:?}]\t{:?}\t[{:?}]", above, &v.debug_id(), below);
    }

    pub fn analyze_anf(&mut self, a: &AnfAnn) -> Result<(), CompileError> {
        use crate::grammar::Anf::*;
        match a {
            AVar(v, s) => match self.above_below.get(v) {
                None => Err(Generic(
                    "impossible: variables should already be established".to_string(),
                )),
                Some(_) => {
                    self.insert_below(v);
                    self.insert_above(v);
                    Ok(())
                }
            },
            AVal(_, b) => Ok(()),
            And(bl, br) => {
                self.analyze_anf(bl)?;
                self.analyze_anf(br)?;
                Ok(())
            }
            Or(bl, br) => {
                self.analyze_anf(bl)?;
                self.analyze_anf(br)?;
                Ok(())
            }
            Neg(bl) => self.analyze_anf(bl),
        }
    }
    /// FIXME: /technically/ this might mess up if there is a prod with many variable refs.
    pub fn analyze_anfs(&mut self, anfs: &[AnfAnn]) -> Result<(), CompileError> {
        for a in anfs {
            self.analyze_anf(a)?;
        }
        Ok(())
    }
    pub fn analyze_expr(&mut self, e: &ExprAnn) -> Result<(), CompileError> {
        use crate::grammar::Expr::*;
        match e {
            EAnf(_, a) => self.analyze_anf(a),
            EPrj(_, i, a) => self.analyze_anf(a),
            EFst(_, a) => self.analyze_anf(a),
            ESnd(_, a) => self.analyze_anf(a),
            EProd(_, anfs) => self.analyze_anfs(anfs),
            ELetIn(v, s, ebound, ebody) => {
                self.insert_above(v);
                self.analyze_expr(ebound)?;
                self.analyze_expr(ebody)?;
                Ok(())
            }
            EIte(_ty, cond, t, f) => {
                self.analyze_anf(cond)?;
                self.analyze_expr(t)?;
                self.analyze_expr(f)?;
                Ok(())
            }
            EFlip(v, param) => {
                self.insert_above(v);
                Ok(())
            }
            EObserve(_, a) => self.analyze_anf(a),
            ESample(_, e) => self.analyze_expr(e),
        }
    }
    pub fn decorate_anf(&mut self, a: &AnfAnn) -> Result<AnfAnlys, CompileError> {
        use crate::grammar::Anf::*;
        match a {
            AVar(v, s) => match self.decor.get(v) {
                None => panic!("impossible"),
                Some(dv) => Ok(AVar(dv.clone(), s.to_string())),
            },
            AVal(_, b) => Ok(AVal((), b.clone())),
            And(bl, br) => Ok(And(
                Box::new(self.decorate_anf(bl)?),
                Box::new(self.decorate_anf(br)?),
            )),
            Or(bl, br) => Ok(Or(
                Box::new(self.decorate_anf(bl)?),
                Box::new(self.decorate_anf(br)?),
            )),
            Neg(bl) => Ok(Neg(Box::new(self.decorate_anf(bl)?))),
        }
    }
    pub fn decorate_anfs(&mut self, anfs: &[AnfAnn]) -> Result<Vec<AnfAnlys>, CompileError> {
        anfs.iter().map(|a| self.decorate_anf(a)).collect()
    }
    pub fn decorate_expr(&mut self, e: &ExprAnn) -> Result<ExprAnlys, CompileError> {
        use crate::grammar::Expr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.decorate_anf(a)?))),
            EPrj(_, i, a) => Ok(EPrj((), *i, Box::new(self.decorate_anf(a)?))),
            EFst(_, a) => Ok(EFst((), Box::new(self.decorate_anf(a)?))),
            ESnd(_, a) => Ok(ESnd((), Box::new(self.decorate_anf(a)?))),
            EProd(_, anfs) => Ok(EProd((), self.decorate_anfs(anfs)?)),
            ELetIn(v, s, ebound, ebody) => match self.decor.get(v) {
                None => panic!("impossible"),
                Some(dv) => Ok(ELetIn(
                    dv.clone(),
                    s.clone(),
                    Box::new(self.decorate_expr(ebound)?),
                    Box::new(self.decorate_expr(ebody)?),
                )),
            },
            EIte(_ty, cond, t, f) => Ok(EIte(
                (),
                Box::new(self.decorate_anf(cond)?),
                Box::new(self.decorate_expr(t)?),
                Box::new(self.decorate_expr(f)?),
            )),

            EFlip(v, param) => match self.decor.get(v) {
                None => panic!("impossible"),
                Some(dv) => Ok(EFlip(dv.clone(), *param)),
            },
            EObserve(_, a) => {
                let anf = self.decorate_anf(a)?;
                Ok(EObserve((), Box::new(anf)))
            }
            ESample(_, e) => Ok(ESample((), Box::new(self.decorate_expr(e)?))),
        }
    }

    pub fn compile_decorations(&mut self) {
        for (var, (above, below)) in self.above_below.clone() {
            let dv = DecoratedVar::new(&var, above, below);
            self.decor.insert(var, dv);
        }
    }

    pub fn interaction_graph(&mut self) -> Result<(), CompileError> {
        Ok(())
    }

    pub fn decorate(
        &mut self,
        p: &ProgramAnn,
        analyze: bool,
    ) -> Result<ProgramAnlys, CompileError> {
        match p {
            Program::Body(e) => {
                if analyze {
                    self.analyze_expr(e)?;
                    self.compile_decorations();
                }
                let fin = self.decorate_expr(e)?;
                Ok(Program::Body(fin))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compile::*;
    use crate::grammar::*;
    use crate::grammar_macros::*;
    use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
    use crate::typecheck::typecheck;
    use crate::*;
    use tracing::*;
    use tracing_test::traced_test;
}
