use crate::annotate::grammar::*;
use crate::compile::CompileError;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use grammar::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use std::collections::{HashMap, HashSet};
pub mod grammar {
    use super::*;
    use std::fmt;
    use std::fmt::*;

    #[derive(Clone, Eq, PartialEq, Debug)]
    pub struct DecoratedVar {
        pub var: Var,
        pub above: HashSet<Var>,
        pub below: HashSet<Var>,
    }

    // impl fmt::Display for Var {
    //     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //         write!(
    //             f,
    //             "Var({:?}->{:?}: {:?}, ",
    //             self.id, self.label, self.provenance
    //         )
    //     }
    // }

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
        type Ext = Var;
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
    lblsym: u64,
    subst_var: HashMap<UniqueId, Var>,
}

impl AnalysisEnv {
    pub fn new() -> Self {
        Self {
            lblsym: 0,
            subst_var: HashMap::new(),
        }
    }
    pub fn max_varlabel_val(&self) -> u64 {
        self.lblsym
    }

    pub fn linear_var_order(&self) -> rsdd::repr::var_order::VarOrder {
        rsdd::repr::var_order::VarOrder::linear_order(self.max_varlabel_val() as usize)
    }
    pub fn get_inv(&self) -> HashMap<VarLabel, Var> {
        let mut inv = HashMap::new();
        for (_, var) in self.subst_var.iter() {
            match var.label {
                Some(label) => inv.insert(label, var.clone()),
                None => continue,
            };
        }
        inv
    }

    fn fresh(&mut self) -> VarLabel {
        let sym = self.lblsym;
        self.lblsym += 1;
        VarLabel::new(sym)
    }

    pub fn get_var(&self, id: &UniqueId) -> Result<Var, CompileError> {
        match self.subst_var.get(id) {
            None => Err(CompileError::Generic(format!("symbol {id} not in scope"))),
            Some(x) => Ok(x.clone()),
        }
    }

    pub fn analyze_anf(&mut self, a: &AnfAnn) -> Result<AnfAnlys, CompileError> {
        use crate::grammar::Anf::*;
        match a {
            AVar(v, s) => Ok(AVar(todo!(), s.to_string())),
            AVal(_, b) => Ok(AVal((), b.clone())),
            And(bl, br) => Ok(And(
                Box::new(self.analyze_anf(bl)?),
                Box::new(self.analyze_anf(br)?),
            )),
            Or(bl, br) => Ok(Or(
                Box::new(self.analyze_anf(bl)?),
                Box::new(self.analyze_anf(br)?),
            )),
            Neg(bl) => Ok(Neg(Box::new(self.analyze_anf(bl)?))),
        }
    }
    pub fn analyze_anfs(&mut self, anfs: &[AnfAnn]) -> Result<Vec<AnfAnlys>, CompileError> {
        anfs.iter().map(|a| self.analyze_anf(a)).collect()
    }
    pub fn analyze_expr(&mut self, e: &ExprAnn) -> Result<ExprAnlys, CompileError> {
        use crate::grammar::Expr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.analyze_anf(a)?))),
            EPrj(_, i, a) => Ok(EPrj((), *i, Box::new(self.analyze_anf(a)?))),
            EFst(_, a) => Ok(EFst((), Box::new(self.analyze_anf(a)?))),
            ESnd(_, a) => Ok(ESnd((), Box::new(self.analyze_anf(a)?))),
            EProd(_, anfs) => Ok(EProd((), self.analyze_anfs(anfs)?)),
            ELetIn(v, s, ebound, ebody) => {
                // let lbl = self.fresh();
                let var = Var::new(todo!(), None, Some(s.to_string()));
                // self.weights.insert(var.clone(), Weight::constant());
                // self.subst_var.insert(*id, var.clone());
                Ok(ELetIn(
                    var,
                    s.clone(),
                    Box::new(self.analyze_expr(ebound)?),
                    Box::new(self.analyze_expr(ebody)?),
                ))
            }
            EIte(_ty, cond, t, f) => Ok(EIte(
                (),
                Box::new(self.analyze_anf(cond)?),
                Box::new(self.analyze_expr(t)?),
                Box::new(self.analyze_expr(f)?),
            )),
            EFlip(v, param) => {
                let lbl = self.fresh();
                let var = Var::new(todo!(), Some(lbl), None);
                // self.subst_var.insert(todo!(), var.clone());
                Ok(EFlip(todo!(), *param))
            }
            EObserve(_, a) => {
                let anf = self.analyze_anf(a)?;
                Ok(EObserve((), Box::new(anf)))
            }
            ESample(_, e) => Ok(ESample((), Box::new(self.analyze_expr(e)?))),
        }
    }

    pub fn analyze(&mut self, p: &ProgramAnn) -> Result<ProgramAnlys, CompileError> {
        match p {
            Program::Body(e) => {
                let eann = self.analyze_expr(e)?;
                Ok(Program::Body(eann))
            }
        }
    }
    pub fn interaction_graph(&mut self) -> Result<(), CompileError> {
        Ok(())
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
