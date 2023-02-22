use crate::compile::CompileError;
use crate::grammar::*;

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

use grammar::*;
pub fn annotate_anf(a: &AnfUD) -> Result<AnfAnn, CompileError> {
    use crate::grammar::ANF::*;
    match a {
        AVar(ty, s) => Ok(AVar(Default::default(), s.clone())),
        AVal(_, v) => Ok(AVal((), v.clone())),
        And(bl, br) => Ok(And(
            Box::new(annotate_anf(bl)?),
            Box::new(annotate_anf(br)?),
        )),
        Or(bl, br) => Ok(Or(Box::new(annotate_anf(bl)?), Box::new(annotate_anf(br)?))),
        Neg(bl) => Ok(Neg(Box::new(annotate_anf(bl)?))),
    }
}
pub fn annotate_anfs(anfs: &Vec<AnfUD>) -> Result<Vec<AnfAnn>, CompileError> {
    anfs.iter().map(annotate_anf).collect()
}

pub fn annotate_expr(e: &ExprUD) -> Result<ExprAnn, CompileError> {
    use crate::grammar::Expr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), Box::new(annotate_anf(a)?))),
        EPrj(_ty, i, a) => Ok(EPrj((), *i, Box::new(annotate_anf(a)?))),
        EFst(_ty, a) => Ok(EFst((), Box::new(annotate_anf(a)?))),
        ESnd(_ty, a) => Ok(ESnd((), Box::new(annotate_anf(a)?))),
        EProd(_ty, anfs) => Ok(EProd((), annotate_anfs(anfs)?)),
        ELetIn(_ty, s, ebound, ebody) => Ok(ELetIn(
            Default::default(),
            s.clone(),
            Box::new(annotate_expr(ebound)?),
            Box::new(annotate_expr(ebody)?),
        )),
        EIte(_ty, cond, t, f) => Ok(EIte(
            Default::default(),
            Box::new(annotate_anf(cond)?),
            Box::new(annotate_expr(t)?),
            Box::new(annotate_expr(f)?),
        )),
        EFlip(_, param) => Ok(EFlip(Default::default(), param.clone())),
        EObserve(_, a) => Ok(EObserve((), Box::new(annotate_anf(a)?))),
        ESample(_, e) => Ok(ESample((), Box::new(annotate_expr(e)?))),
    }
}

pub fn annotate(p: &ProgramUD) -> Result<ProgramAnn, CompileError> {
    match p {
        Program::Body(e) => Ok(Program::Body(annotate_expr(e)?)),
    }
}
