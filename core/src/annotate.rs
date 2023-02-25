use crate::compile::CompileError;
use crate::grammar::*;
use crate::uniquify::grammar::*;
use grammar::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use std::collections::HashMap;
pub mod grammar {
    use super::*;
    use std::fmt;
    use std::fmt::*;

    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    pub struct Var {
        pub id: UniqueId,               // associated unique ids
        pub label: Option<VarLabel>,    // only hold values in the final formula
        pub provenance: Option<String>, // when None, this indicates that the variable is in the final formula
    }
    impl Var {
        pub fn new(id: UniqueId, label: Option<VarLabel>, provenance: Option<String>) -> Self {
            Self {
                id,
                label,
                provenance,
            }
        }
    }

    impl fmt::Display for Var {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "Var({:?}->{:?}: {:?}, ",
                self.id, self.label, self.provenance
            )
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Annotated;

    impl ξ<Annotated> for AVarExt {
        // vars up/down
        // Vars are "sample-able"
        type Ext = Var;
    }
    impl ξ<Annotated> for AValExt {
        type Ext = ();
    }
    pub type AnfAnn = ANF<Annotated>;

    impl ξ<Annotated> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Annotated> for EFstExt {
        type Ext = ();
    }
    impl ξ<Annotated> for ESndExt {
        type Ext = ();
    }
    impl ξ<Annotated> for EPrjExt {
        // sampleable
        type Ext = ();
    }
    impl ξ<Annotated> for EProdExt {
        type Ext = ();
    }
    impl ξ<Annotated> for ELetInExt {
        // vars up/down
        // binders are "sample-able"
        type Ext = Var;
    }
    impl ξ<Annotated> for EIteExt {
        type Ext = ();
    }
    impl ξ<Annotated> for EFlipExt {
        // vars up/down
        // flip is sample-able
        type Ext = Var;
    }
    impl ξ<Annotated> for EObserveExt {
        type Ext = ();
    }
    impl ξ<Annotated> for ESampleExt {
        type Ext = ();
    }

    pub type ExprAnn = Expr<Annotated>;
    pub type ProgramAnn = Program<Annotated>;
}

pub struct LabelEnv {
    lblsym: u64,
    subst_var: HashMap<UniqueId, Var>,
}

impl LabelEnv {
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
                Some(label) => inv.insert(label.clone(), var.clone()),
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

    pub fn annotate_anf(&mut self, a: &AnfUnq) -> Result<AnfAnn, CompileError> {
        use crate::grammar::ANF::*;
        match a {
            AVar(uid, s) => Ok(AVar(self.get_var(uid)?, s.to_string())),
            AVal(_, b) => Ok(AVal((), b.clone())),
            And(bl, br) => Ok(And(
                Box::new(self.annotate_anf(&*bl)?),
                Box::new(self.annotate_anf(br)?),
            )),
            Or(bl, br) => Ok(Or(
                Box::new(self.annotate_anf(bl)?),
                Box::new(self.annotate_anf(br)?),
            )),
            Neg(bl) => Ok(Neg(Box::new(self.annotate_anf(bl)?))),
        }
    }
    pub fn annotate_anfs(&mut self, anfs: &Vec<AnfUnq>) -> Result<Vec<AnfAnn>, CompileError> {
        anfs.iter().map(|a| self.annotate_anf(a)).collect()
    }
    pub fn annotate_expr(&mut self, e: &ExprUnq) -> Result<ExprAnn, CompileError> {
        use crate::grammar::Expr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.annotate_anf(a)?))),
            EPrj(_, i, a) => Ok(EPrj((), *i, Box::new(self.annotate_anf(a)?))),
            EFst(_, a) => Ok(EFst((), Box::new(self.annotate_anf(a)?))),
            ESnd(_, a) => Ok(ESnd((), Box::new(self.annotate_anf(a)?))),
            EProd(_, anfs) => Ok(EProd((), self.annotate_anfs(anfs)?)),
            ELetIn(id, s, ebound, ebody) => {
                // let lbl = self.fresh();
                let var = Var::new(*id, None, Some(s.to_string()));
                // self.weights.insert(var.clone(), Weight::constant());
                self.subst_var.insert(*id, var.clone());
                Ok(ELetIn(
                    var,
                    s.clone(),
                    Box::new(self.annotate_expr(ebound)?),
                    Box::new(self.annotate_expr(ebody)?),
                ))
            }
            EIte(_ty, cond, t, f) => Ok(EIte(
                (),
                Box::new(self.annotate_anf(cond)?),
                Box::new(self.annotate_expr(t)?),
                Box::new(self.annotate_expr(f)?),
            )),
            EFlip(id, param) => {
                let lbl = self.fresh();
                let var = Var::new(*id, Some(lbl), None);
                self.subst_var.insert(*id, var.clone());
                Ok(EFlip(var, param.clone()))
            }
            EObserve(_, a) => {
                let anf = self.annotate_anf(a)?;
                Ok(EObserve((), Box::new(anf)))
            }
            ESample(_, e) => Ok(ESample((), Box::new(self.annotate_expr(e)?))),
        }
    }

    pub fn annotate(
        &mut self,
        p: &ProgramUnq,
    ) -> Result<
        (
            ProgramAnn,
            VarOrder,
            HashMap<UniqueId, Var>,
            HashMap<VarLabel, Var>,
            u64,
        ),
        CompileError,
    > {
        match p {
            Program::Body(e) => {
                let eann = self.annotate_expr(e)?;
                let order = self.linear_var_order();
                let inv = self.get_inv();
                let mx = self.max_varlabel_val();
                Ok((Program::Body(eann), order, self.subst_var.clone(), inv, mx))
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
