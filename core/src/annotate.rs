use crate::compile::CompileError;
use crate::grammar::*;
use crate::uniquify::grammar::*;
use grammar::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use std::collections::HashMap;
use std::collections::HashSet;

pub type InvMap = HashMap<NamedVar, HashSet<BddVar>>;

pub mod grammar {
    use super::*;
    use std::fmt;
    use std::fmt::*;

    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    pub struct BddVar {
        pub id: UniqueId,
        pub label: VarLabel,
        pub provenance: Option<NamedVar>,
    }
    impl BddVar {
        pub fn id(&self) -> UniqueId {
            self.id
        }
    }

    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    pub struct NamedVar {
        pub id: UniqueId,
        pub name: String,
    }
    impl NamedVar {
        pub fn id(&self) -> UniqueId {
            self.id
        }
    }

    // FIXME: this should be an enum of BddVar or NamedVar
    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    pub enum Var {
        Bdd(BddVar),
        Named(NamedVar),
    }
    impl Var {
        pub fn new_bdd(id: UniqueId, label: VarLabel, provenance: Option<NamedVar>) -> Self {
            Var::Bdd(BddVar {
                id,
                label,
                provenance,
            })
        }
        pub fn new_named(id: UniqueId, name: String) -> Self {
            Var::Named(NamedVar { id, name })
        }
        pub fn debug_id(&self) -> String {
            match self {
                Var::Bdd(v) => format!("{}", v.id),
                Var::Named(v) => v.name.clone(),
            }
        }
        pub fn unsafe_label(&self) -> VarLabel {
            match self {
                Var::Bdd(v) => v.label,
                Var::Named(_) => panic!("shame on you!"),
            }
        }
        pub fn id(&self) -> UniqueId {
            match self {
                Var::Bdd(v) => v.id,
                Var::Named(v) => v.id,
            }
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
    pub type AnfAnn = Anf<Annotated>;

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
    letpos: Option<NamedVar>,
}

impl LabelEnv {
    pub fn new() -> Self {
        Self {
            lblsym: 0,
            subst_var: HashMap::new(),
            letpos: None,
        }
    }
    pub fn max_varlabel_val(&self) -> u64 {
        self.lblsym
    }

    pub fn linear_var_order(&self) -> rsdd::repr::var_order::VarOrder {
        rsdd::repr::var_order::VarOrder::linear_order(self.max_varlabel_val() as usize)
    }
    pub fn get_inv(&self) -> HashMap<NamedVar, HashSet<BddVar>> {
        let mut inv: HashMap<NamedVar, HashSet<BddVar>> = HashMap::new();
        for (_, var) in self.subst_var.iter() {
            match &var {
                Var::Named(_) => continue,
                Var::Bdd(v) => match &v.provenance {
                    None => continue,
                    Some(prov) => match inv.get_mut(&prov) {
                        None => {
                            inv.insert(prov.clone(), HashSet::from([v.clone()]));
                        }
                        Some(vs) => {
                            vs.insert(v.clone());
                        }
                    },
                },
            }
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
        use crate::grammar::Anf::*;
        match a {
            AVar(uid, s) => Ok(AVar(self.get_var(uid)?, s.to_string())),
            AVal(_, b) => Ok(AVal((), b.clone())),
            And(bl, br) => Ok(And(
                Box::new(self.annotate_anf(bl)?),
                Box::new(self.annotate_anf(br)?),
            )),
            Or(bl, br) => Ok(Or(
                Box::new(self.annotate_anf(bl)?),
                Box::new(self.annotate_anf(br)?),
            )),
            Neg(bl) => Ok(Neg(Box::new(self.annotate_anf(bl)?))),
        }
    }
    pub fn annotate_anfs(&mut self, anfs: &[AnfUnq]) -> Result<Vec<AnfAnn>, CompileError> {
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
                let nvar = NamedVar {
                    id: *id,
                    name: s.to_string(),
                };
                let var = Var::Named(nvar.clone());
                self.letpos = Some(nvar);
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
                let var = Var::new_bdd(*id, lbl, self.letpos.clone());
                self.subst_var.insert(*id, var.clone());
                Ok(EFlip(var, *param))
            }
            EObserve(_, a) => {
                let anf = self.annotate_anf(a)?;
                Ok(EObserve((), Box::new(anf)))
            }
            ESample(_, e) => Ok(ESample((), Box::new(self.annotate_expr(e)?))),
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn annotate(
        &mut self,
        p: &ProgramUnq,
    ) -> Result<
        (
            ProgramAnn,
            VarOrder,
            HashMap<UniqueId, Var>,
            HashMap<NamedVar, HashSet<BddVar>>,
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
