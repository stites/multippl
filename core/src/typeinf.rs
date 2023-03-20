use crate::compile::CompileError;
use crate::grammar::*;
use crate::typecheck::grammar::{AnfTyped, ExprTyped, LetInTypes, ProgramTyped};

pub mod grammar {
    use super::*;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Inferable;

    impl ξ<Inferable> for AVarExt {
        type Ext = Option<Ty>;
    }
    impl ξ<Inferable> for AValExt {
        type Ext = ();
    }
    impl ξ<Inferable> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Inferable> for EFstExt {
        type Ext = Option<Ty>;
    }
    impl ξ<Inferable> for ESndExt {
        type Ext = Option<Ty>;
    }
    impl ξ<Inferable> for EPrjExt {
        type Ext = Option<Ty>;
    }
    impl ξ<Inferable> for EProdExt {
        type Ext = Option<Ty>;
    }
    impl ξ<Inferable> for ELetInExt {
        type Ext = Option<LetInTypes>;
    }
    impl ξ<Inferable> for EIteExt {
        type Ext = Option<Ty>;
    }
    impl ξ<Inferable> for EFlipExt {
        type Ext = ();
    }
    impl ξ<Inferable> for EObserveExt {
        type Ext = ();
    }
    impl ξ<Inferable> for ESampleExt {
        type Ext = ();
    }

    pub type AnfInferable = Anf<Inferable>;
    pub type ExprInferable = Expr<Inferable>;
    pub type ProgramInferable = Program<Inferable>;
}

pub fn typeinference_anf(a: &grammar::AnfInferable) -> Result<AnfTyped, CompileError> {
    use crate::grammar::Anf::*;
    match a {
        AVar(ty, s) => Ok(AVar(Ty::Bool, s.clone())),
        AVal(_, v) => Ok(AVal((), v.clone())),
        And(bl, br) => Ok(And(
            Box::new(typeinference_anf(bl)?),
            Box::new(typeinference_anf(br)?),
        )),
        Or(bl, br) => Ok(Or(
            Box::new(typeinference_anf(bl)?),
            Box::new(typeinference_anf(br)?),
        )),
        Neg(bl) => Ok(Neg(Box::new(typeinference_anf(bl)?))),
    }
}
pub fn typeinference_anfs(anfs: &[grammar::AnfInferable]) -> Result<Vec<AnfTyped>, CompileError> {
    anfs.iter().map(typeinference_anf).collect()
}
fn ignored_type() -> Ty {
    Ty::Bool
}

pub fn typeinference_expr(e: &grammar::ExprInferable) -> Result<ExprTyped, CompileError> {
    use crate::grammar::Expr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), Box::new(typeinference_anf(a)?))),
        EPrj(_ty, i, a) => {
            // ignore types for now.
            Ok(EPrj(ignored_type(), *i, Box::new(typeinference_anf(a)?)))
        }
        EFst(_ty, a) => Ok(EFst(ignored_type(), Box::new(typeinference_anf(a)?))),
        ESnd(_ty, a) => Ok(ESnd(ignored_type(), Box::new(typeinference_anf(a)?))),
        EProd(_ty, anfs) => Ok(EProd(ignored_type(), typeinference_anfs(anfs)?)),
        ELetIn(_ty, s, ebound, ebody) => Ok(ELetIn(
            LetInTypes {
                bindee: ignored_type(),
                body: ignored_type(),
            },
            s.clone(),
            Box::new(typeinference_expr(ebound)?),
            Box::new(typeinference_expr(ebody)?),
        )),
        EIte(_ty, cond, t, f) => Ok(EIte(
            ignored_type(),
            Box::new(typeinference_anf(cond)?),
            Box::new(typeinference_expr(t)?),
            Box::new(typeinference_expr(f)?),
        )),
        EFlip(_, param) => Ok(EFlip((), *param)),
        EObserve(_, a) => Ok(EObserve((), Box::new(typeinference_anf(a)?))),
        ESample(_, e) => Ok(ESample((), Box::new(typeinference_expr(e)?))),
    }
}

pub fn typeinference(p: &grammar::ProgramInferable) -> Result<ProgramTyped, CompileError> {
    match p {
        Program::Body(e) => Ok(Program::Body(typeinference_expr(e)?)),
    }
}

pub fn pipeline(p: &grammar::ProgramInferable) -> Result<ProgramTyped, CompileError> {
    typeinference(p)
}
