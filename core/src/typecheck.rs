use crate::compile::CompileError;
use crate::grammar::*;

pub mod grammar {
    use super::*;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Typed;
    #[derive(Debug, PartialEq, Clone)]
    pub struct LetInTypes {
        pub bindee: Ty,
        pub body: Ty,
    }
    impl ξ<Typed> for AVarExt {
        type Ext = Ty;
    }
    impl ξ<Typed> for AValExt {
        type Ext = ();
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

    pub type AnfTyped = Anf<Typed>;
    pub type ExprTyped = Expr<Typed>;
    pub type ProgramTyped = Program<Typed>;
    impl AnfTyped {
        pub fn as_type(&self) -> Ty {
            use Anf::*;
            match self {
                AVar(t, _) => t.clone(),
                AVal(_, v) => v.as_type(),
                _ => Ty::Bool,
            }
        }
        pub fn is_type(&self, ty: &Ty) -> bool {
            self.as_type() == *ty
        }
    }
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

pub fn typecheck_anf(a: &grammar::AnfTyped) -> Result<AnfUD, CompileError> {
    use crate::grammar::Anf::*;
    match a {
        AVar(ty, s) => {
            // if !ctx.gamma.typechecks(s.clone(), ty) {
            //     Err(TypeError(format!(
            //         "Expected {s} : {ty:?}\nGot: {a:?}\n{ctx:?}",
            //     )))
            // } else {
            Ok(AVar((), s.clone()))
        }
        AVal(_, v) => Ok(AVal((), v.clone())),
        And(bl, br) => Ok(And(
            Box::new(typecheck_anf(bl)?),
            Box::new(typecheck_anf(br)?),
        )),
        Or(bl, br) => Ok(Or(
            Box::new(typecheck_anf(bl)?),
            Box::new(typecheck_anf(br)?),
        )),
        Neg(bl) => Ok(Neg(Box::new(typecheck_anf(bl)?))),
    }
}
pub fn typecheck_anfs(anfs: &[grammar::AnfTyped]) -> Result<Vec<AnfUD>, CompileError> {
    anfs.iter().map(typecheck_anf).collect()
}

pub fn typecheck_expr(e: &grammar::ExprTyped) -> Result<ExprUD, CompileError> {
    use crate::grammar::Expr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), Box::new(typecheck_anf(a)?))),
        EPrj(_ty, i, a) => {
            // ignore types for now.
            // let aty = a.as_type();
            // assert!(aty.left() == Some(*ty.clone()), "actual {:?} != expected {:?}. type is: {:?}", aty.left(), Some(*ty.clone()), ty);
            Ok(EPrj((), *i, Box::new(typecheck_anf(a)?)))
        }
        EFst(_ty, a) => Ok(EFst((), Box::new(typecheck_anf(a)?))),
        ESnd(_ty, a) => Ok(ESnd((), Box::new(typecheck_anf(a)?))),
        EProd(_ty, anfs) => Ok(EProd((), typecheck_anfs(anfs)?)),
        ELetIn(_ty, s, ebound, ebody) => Ok(ELetIn(
            (),
            s.clone(),
            Box::new(typecheck_expr(ebound)?),
            Box::new(typecheck_expr(ebody)?),
        )),
        EIte(_ty, cond, t, f) => Ok(EIte(
            (),
            Box::new(typecheck_anf(cond)?),
            Box::new(typecheck_expr(t)?),
            Box::new(typecheck_expr(f)?),
        )),
        EFlip(_, param) => Ok(EFlip((), *param)),
        EObserve(_, a) => Ok(EObserve((), Box::new(typecheck_anf(a)?))),
        ESample(_, e) => Ok(ESample((), Box::new(typecheck_expr(e)?))),
    }
}

pub fn typecheck(p: &grammar::ProgramTyped) -> Result<ProgramUD, CompileError> {
    match p {
        Program::Body(e) => Ok(Program::Body(typecheck_expr(e)?)),
    }
}
