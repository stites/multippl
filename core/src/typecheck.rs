use crate::compile::{
    grammar::{ExprUD, ProgramUD},
    semantics::CompileError,
};
use crate::grammar::*;

pub mod grammar {
    use super::*;

    pub struct Typed;
    #[derive(Debug, PartialEq, Clone)]
    pub struct LetInTypes {
        pub bindee: Ty,
        pub body: Ty,
    }
    impl ξ<Typed> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Typed> for EFstExt {
        type Ext = Ty;
    }
    impl ξ<Typed> for ESndExt {
        type Ext = Ty;
    }
    impl ξ<Typed> for EPrjExt {
        type Ext = Ty;
    }
    impl ξ<Typed> for EProdExt {
        type Ext = Ty;
    }
    impl ξ<Typed> for ELetInExt {
        type Ext = LetInTypes;
    }
    impl ξ<Typed> for EIteExt {
        type Ext = Ty;
    }
    impl ξ<Typed> for EFlipExt {
        type Ext = ();
    }
    impl ξ<Typed> for EObserveExt {
        type Ext = ();
    }
    impl ξ<Typed> for ESampleExt {
        type Ext = ();
    }

    pub type ExprTyped = Expr<Typed>;
    pub type ProgramTyped = Program<Typed>;
}

pub fn is_type(e: &grammar::ExprTyped, ty: &Ty) -> bool {
    as_type(e) == *ty
}
pub fn as_type(e: &grammar::ExprTyped) -> Ty {
    use Expr::*;
    match e {
        EAnf(_, anf) => anf.as_type(),
        EFst(t, _) => t.clone(),
        ESnd(t, _) => t.clone(),
        EPrj(t, _, _) => t.clone(),
        EProd(t, _) => t.clone(),
        ELetIn(t, _, _, _) => t.body.clone(),
        EIte(t, _, _, _) => t.clone(),
        EFlip(_, _) => Ty::Bool,
        EObserve(_, _) => Ty::Bool,
        ESample(_, _) => Ty::Bool,
    }
}

pub fn typecheck_expr(e: &grammar::ExprTyped) -> Result<ExprUD, CompileError> {
    use crate::grammar::Expr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), a.clone())),
        EPrj(_ty, i, a) => {
            // ignore types for now.
            // let aty = a.as_type();
            // assert!(aty.left() == Some(*ty.clone()), "actual {:?} != expected {:?}. type is: {:?}", aty.left(), Some(*ty.clone()), ty);
            Ok(EPrj((), *i, a.clone()))
        }
        EFst(_ty, a) => Ok(EFst((), a.clone())),
        ESnd(_ty, a) => Ok(ESnd((), a.clone())),
        EProd(_ty, anfs) => Ok(EProd((), anfs.clone())),
        ELetIn(_ty, s, ebound, ebody) => Ok(ELetIn(
            (),
            s.clone(),
            Box::new(typecheck_expr(ebound)?),
            Box::new(typecheck_expr(ebody)?),
        )),
        EIte(_ty, cond, t, f) => Ok(EIte(
            (),
            cond.clone(),
            Box::new(typecheck_expr(t)?),
            Box::new(typecheck_expr(f)?),
        )),
        EFlip(_, param) => Ok(EFlip((), param.clone())),
        EObserve(_, a) => Ok(EObserve((), a.clone())),
        ESample(_, e) => Ok(ESample((), Box::new(typecheck_expr(e)?))),
    }
}

pub fn typecheck(p: &grammar::ProgramTyped) -> Result<ProgramUD, CompileError> {
    match p {
        Program::Body(e) => Ok(Program::Body(typecheck_expr(e)?)),
    }
}
