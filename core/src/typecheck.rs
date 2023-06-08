use crate::data::CompileError;
use crate::grammar::*;

pub mod grammar {
    use super::*;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Typed;
    #[derive(Debug, PartialEq, Clone)]
    pub struct LetInTypes<T> {
        pub bindee: T,
        pub body: T,
    }
    impl ξ<Typed> for AVarExt<ETy> {
        type Ext = ETy;
    }
    impl ξ<Typed> for AVarExt<STy> {
        type Ext = STy;
    }
    impl ξ<Typed> for AValExt {
        type Ext = ();
    }
    impl ξ<Typed> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Typed> for EPrjExt {
        type Ext = ETy;
    }
    impl ξ<Typed> for EProdExt {
        type Ext = ETy;
    }
    impl ξ<Typed> for ELetInExt {
        type Ext = LetInTypes<ETy>;
    }
    impl ξ<Typed> for EIteExt {
        type Ext = ETy;
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
    impl ξ<Typed> for ESample2Ext {
        type Ext = ();
    }
    impl ξ<Typed> for SAnfExt {
        type Ext = ();
    }
    impl ξ<Typed> for SLetInExt {
        type Ext = LetInTypes<ETy>;
    }
    impl ξ<Typed> for SSeqExt {
        type Ext = ();
    }
    impl ξ<Typed> for SIteExt {
        type Ext = ();
    }
    impl ξ<Typed> for SFlipExt {
        type Ext = ();
    }
    impl ξ<Typed> for SExactExt {
        type Ext = ();
    }

    pub type AnfTyped<X> = Anf<Typed, X>;
    pub type EExprTyped = EExpr<Typed>;
    pub type SExprTyped = SExpr<Typed>;
    pub type ProgramTyped = Program<Typed>;
    impl <X:grammar::IsTyped<T>> AnfTyped<X> {
        pub fn as_type(&self) -> ETy {
            use Anf::*;
            match self {
                AVar(t, _) => t.clone(),
                AVal(_, v) => v.as_type(),
                _ => ETy::EBool,
            }
        }
        pub fn is_type(&self, ty: &ETy) -> bool {
            self.as_type() == *ty
        }
        pub fn var(s: String) -> AnfTyped<X> {
            Anf::AVar(ETy::EBool, s)
        }
    }
}

pub fn is_type(e: &grammar::EExprTyped, ty: &ETy) -> bool {
    as_type(e) == *ty
}
pub fn as_type(e: &grammar::EExprTyped) -> ETy {
    use EExpr::*;
    match e {
        EAnf(_, anf) => anf.as_type(),
        EPrj(t, _, _) => t.clone(),
        EProd(t, _) => t.clone(),
        ELetIn(t, _, _, _) => t.body.clone(),
        EIte(t, _, _, _) => t.clone(),
        EFlip(_, _) => ETy::EBool,
        EObserve(_, _) => ETy::EBool,
        ESample(_, _) => ETy::EBool,
        ESample2(_, e) => todo!(),
    }
}

pub fn typecheck_anf<X:Clone>(a: &grammar::AnfTyped<X>) -> Result<AnfUD<X>, CompileError> {
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
pub fn typecheck_anfs<X:Clone>(anfs: &[grammar::AnfTyped<X>]) -> Result<Vec<AnfUD<X>>, CompileError> {
    anfs.iter().map(typecheck_anf).collect()
}

pub fn typecheck_expr(e: &grammar::EExprTyped) -> Result<EExprUD, CompileError> {
    use crate::grammar::EExpr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), Box::new(typecheck_anf(a)?))),
        EPrj(_ty, i, a) => {
            // ignore types for now.
            // let aty = a.as_type();
            // assert!(aty.left() == Some(*ty.clone()), "actual {:?} != expected {:?}. type is: {:?}", aty.left(), Some(*ty.clone()), ty);
            Ok(EPrj((), *i, Box::new(typecheck_anf(a)?)))
        }
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
        ESample2(_, e) => todo!(),
    }
}

pub fn typecheck(p: &grammar::ProgramTyped) -> Result<ProgramUD, CompileError> {
    match p {
        Program::EBody(e) => Ok(Program::EBody(typecheck_expr(e)?)),
    }
}

pub fn pipeline(p: &crate::typeinf::grammar::ProgramInferable) -> Result<ProgramUD, CompileError> {
    typecheck(&crate::typeinf::pipeline(p)?)
}
