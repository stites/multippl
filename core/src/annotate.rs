use crate::compile::CompileError;
use crate::grammar::*;
use grammar::*;
use std::collections::HashMap;

pub mod grammar {
    use super::*;
    use rsdd::repr::var_label::*;
    use std::fmt;
    use std::fmt::*;

    #[derive(Clone, Copy, Eq, Hash, PartialEq, Debug)]
    pub struct UniqueId(pub u64);
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

    #[derive(Debug, PartialEq, Clone)]
    pub struct Annotated;
    #[derive(Debug, PartialEq, Clone)]
    pub struct Variables {
        pub above: Vec<UniqueId>,
        pub below: Vec<UniqueId>,
        pub id: Option<UniqueId>,
    }
    impl Default for Variables {
        fn default() -> Self {
            Variables {
                above: Default::default(),
                below: Default::default(),
                id: Default::default(),
            }
        }
    }

    impl ξ<Annotated> for AVarExt {
        // vars up/down
        // Vars are "sample-able"
        type Ext = Variables;
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
        type Ext = Variables;
    }
    impl ξ<Annotated> for EIteExt {
        // branches are "sample-able"
        type Ext = Variables;
    }
    impl ξ<Annotated> for EFlipExt {
        // vars up/down
        // flip is sample-able
        type Ext = Variables;
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

pub struct SymEnv {
    pub names: HashMap<String, UniqueId>,
    pub gensym: u64,
}
impl Default for SymEnv {
    fn default() -> Self {
        Self {
            names: Default::default(),
            gensym: 0,
        }
    }
}

impl SymEnv {
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
    pub fn annotate_anf(&mut self, a: &AnfUD) -> Result<AnfAnn, CompileError> {
        use crate::grammar::ANF::*;
        match a {
            AVar(_, s) => Ok(AVar(Default::default(), s.clone())),
            AVal(_, v) => Ok(AVal((), v.clone())),
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
    pub fn annotate_anfs(&mut self, anfs: &Vec<AnfUD>) -> Result<Vec<AnfAnn>, CompileError> {
        anfs.iter().map(|a| self.annotate_anf(a)).collect()
    }

    pub fn annotate_expr(&mut self, e: &ExprUD) -> Result<ExprAnn, CompileError> {
        use crate::grammar::Expr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.annotate_anf(a)?))),
            EPrj(_ty, i, a) => Ok(EPrj((), *i, Box::new(self.annotate_anf(a)?))),
            EFst(_ty, a) => Ok(EFst((), Box::new(self.annotate_anf(a)?))),
            ESnd(_ty, a) => Ok(ESnd((), Box::new(self.annotate_anf(a)?))),
            EProd(_ty, anfs) => Ok(EProd((), self.annotate_anfs(anfs)?)),
            ELetIn(_ty, s, ebound, ebody) => Ok(ELetIn(
                Default::default(),
                s.clone(),
                Box::new(self.annotate_expr(ebound)?),
                Box::new(self.annotate_expr(ebody)?),
            )),
            EIte(_ty, cond, t, f) => Ok(EIte(
                Default::default(),
                Box::new(self.annotate_anf(cond)?),
                Box::new(self.annotate_expr(t)?),
                Box::new(self.annotate_expr(f)?),
            )),
            EFlip(_, param) => Ok(EFlip(Default::default(), param.clone())),
            EObserve(_, a) => Ok(EObserve((), Box::new(self.annotate_anf(a)?))),
            ESample(_, e) => Ok(ESample((), Box::new(self.annotate_expr(e)?))),
        }
    }

    pub fn annotate(&mut self, p: &ProgramUD) -> Result<ProgramAnn, CompileError> {
        match p {
            Program::Body(e) => Ok(Program::Body(self.annotate_expr(e)?)),
        }
    }
}

#[cfg(test)]
mod active_tests {
    use crate::compile::*;
    use crate::grammar::*;
    use crate::grammar_macros::*;
    use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
    use crate::typecheck::typecheck;
    use crate::*;
    use tracing::*;
    use tracing_test::traced_test;

    #[test]
    #[traced_test]
    fn free_variables_ids() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "x" ; b!() ;= flip!(1/3);
                "y" ; b!() ;= sample!(
                    lets![
                        "x0" ; b!() ;= flip!(1/5);
                        ...? b!("x0" || "x") ; b!()
                    ]);
               "_" ; b!() ;= observe!(b!("x" || "y")); // is this a problem?

               ...? ret ; b!()
            ])
        };
        let mut senv = SymEnv::default();
        let res = senv
            .annotate(&typecheck(&mk(q!("x" x "l"))).unwrap())
            .unwrap();
        debug!("{:?}", res);

        // check_invariant("free2/x,l ", None, None, &mk(q!("x" x "l")));
    }
}
