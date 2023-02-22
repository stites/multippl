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
    pub struct Uniquify;
    // #[derive(Debug, PartialEq, Clone)]
    // pub struct Variables {
    //     pub above: Vec<UniqueId>,
    //     pub below: Vec<UniqueId>,
    //     pub id: UniqueId,
    // }
    // impl Variables {
    //     pub fn new(id: UniqueId) -> Variables {
    //         Variables {
    //             id,
    //             above: Default::default(),
    //             below: Default::default(),
    //         }
    //     }
    // }
    impl ξ<Uniquify> for AVarExt {
        // vars up/down
        // Vars are "sample-able"
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for AValExt {
        type Ext = ();
    }
    pub type AnfUnq = ANF<Uniquify>;

    impl ξ<Uniquify> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for EFstExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for ESndExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for EPrjExt {
        // sampleable
        type Ext = ();
    }
    impl ξ<Uniquify> for EProdExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for ELetInExt {
        // vars up/down
        // binders are "sample-able"
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for EIteExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for EFlipExt {
        // vars up/down
        // flip is sample-able
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for EObserveExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for ESampleExt {
        type Ext = ();
    }

    pub type ExprUnq = Expr<Uniquify>;
    pub type ProgramUnq = Program<Uniquify>;
}

pub struct SymEnv {
    pub names: HashMap<String, UniqueId>,
    pub gensym: u64,
    read_only: bool,
}
impl Default for SymEnv {
    fn default() -> Self {
        Self {
            names: Default::default(),
            gensym: 0,
            read_only: true,
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
    fn get_or_create(&mut self, var: String) -> Result<UniqueId, CompileError> {
        let osym = self.get_var(var.clone());
        match osym {
            None => {
                if self.read_only {
                    Err(CompileError::Generic(format!(
                        "error: encountered unbound variable \"{}\"",
                        var
                    )))
                } else {
                    Ok(self._fresh(Some(var)))
                }
            }
            Some(sym) => Ok(sym),
        }
    }
    pub fn annotate_anf(&mut self, a: &AnfUD) -> Result<AnfUnq, CompileError> {
        use crate::grammar::ANF::*;
        match a {
            AVar(_, s) => {
                let uid = self.get_or_create(s.to_string())?;
                Ok(AVar(uid, s.to_string()))
            }
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
    pub fn annotate_anfs(&mut self, anfs: &Vec<AnfUD>) -> Result<Vec<AnfUnq>, CompileError> {
        anfs.iter().map(|a| self.annotate_anf(a)).collect()
    }

    pub fn annotate_expr(&mut self, e: &ExprUD) -> Result<ExprUnq, CompileError> {
        use crate::grammar::Expr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.annotate_anf(a)?))),
            EPrj(_ty, i, a) => Ok(EPrj((), *i, Box::new(self.annotate_anf(a)?))),
            EFst(_ty, a) => Ok(EFst((), Box::new(self.annotate_anf(a)?))),
            ESnd(_ty, a) => Ok(ESnd((), Box::new(self.annotate_anf(a)?))),
            EProd(_ty, anfs) => Ok(EProd((), self.annotate_anfs(anfs)?)),
            ELetIn(_ty, s, ebound, ebody) => {
                // too lazy to do something smarter
                self.read_only = false;
                let v = self.get_or_create(s.to_string())?;
                self.read_only = true;
                Ok(ELetIn(
                    v,
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
            EFlip(_, param) => Ok(EFlip(self.fresh(), param.clone())),
            EObserve(_, a) => {
                let anf = self.annotate_anf(a)?;
                Ok(EObserve((), Box::new(anf)))
            }
            ESample(_, e) => Ok(ESample((), Box::new(self.annotate_expr(e)?))),
        }
    }

    pub fn annotate(&mut self, p: &ProgramUD) -> Result<ProgramUnq, CompileError> {
        match p {
            Program::Body(e) => {
                let eann = self.annotate_expr(e)?;
                Ok(Program::Body(eann))
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

    #[test]
    fn invalid_observe() {
        let res = SymEnv::default().annotate(&typecheck(&program!(observe!(b!("x")))).unwrap());
        assert!(res.is_err());
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
        let res = SymEnv::default().annotate(&typecheck(&mk(b!("l"))).unwrap());
        assert!(res.is_err());
        let mut senv = SymEnv::default();
        let res = senv.annotate(&typecheck(&mk(b!("x"))).unwrap());
        assert!(res.is_ok());
        assert!(senv.gensym == 6);
    }
}
