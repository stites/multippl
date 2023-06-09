use crate::data::errors::{CompileError, Result};
use crate::grammar::*;
use crate::typecheck::grammar::{AnfTyped, EExprTyped, LetInTypes, ProgramTyped, SExprTyped};

pub mod grammar {
    use super::*;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Inferable;

    impl ξ<Inferable> for AVarExt<EVal> {
        type Ext = Option<ETy>;
    }
    impl ξ<Inferable> for AValExt<EVal> {
        type Ext = ();
    }
    impl ξ<Inferable> for AVarExt<SVal> {
        type Ext = Option<STy>;
    }
    impl ξ<Inferable> for AValExt<SVal> {
        type Ext = ();
    }
    impl ξ<Inferable> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Inferable> for EPrjExt {
        type Ext = Option<ETy>;
    }
    impl ξ<Inferable> for EProdExt {
        type Ext = Option<ETy>;
    }
    impl ξ<Inferable> for ELetInExt {
        type Ext = Option<LetInTypes<ETy>>;
    }
    impl ξ<Inferable> for EIteExt {
        type Ext = Option<ETy>;
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
    impl ξ<Inferable> for ESample2Ext {
        type Ext = ();
    }
    impl ξ<Inferable> for SAnfExt {
        type Ext = ();
    }
    impl ξ<Inferable> for SLetInExt {
        type Ext = Option<LetInTypes<STy>>;
    }
    impl ξ<Inferable> for SSeqExt {
        type Ext = ();
    }
    impl ξ<Inferable> for SIteExt {
        type Ext = Option<STy>;
    }
    impl ξ<Inferable> for SFlipExt {
        type Ext = ();
    }
    impl ξ<Inferable> for SExactExt {
        type Ext = ();
    }

    pub type AnfInferable<X> = Anf<Inferable, X>;
    pub type EExprInferable = EExpr<Inferable>;
    pub type SExprInferable = SExpr<Inferable>;
    pub type ProgramInferable = Program<Inferable>;
}

use crate::typecheck::grammar::Typed;
use crate::typeinf::grammar::Inferable;

pub fn typeinference_anf<T: Clone, X: Clone>(
    ty: &T,
    a: &grammar::AnfInferable<X>,
) -> Result<AnfTyped<X>>
where
    AVarExt<X>: ξ<Inferable, Ext = Option<T>> + ξ<Typed, Ext = T>,
    AValExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
{
    use crate::grammar::Anf::*;
    match a {
        AVar(_, s) => Ok(AVar(ty.clone(), s.clone())),
        AVal(_, v) => Ok(AVal((), v.clone())),
        And(bl, br) => Ok(And(
            Box::new(typeinference_anf(ty, bl)?),
            Box::new(typeinference_anf(ty, br)?),
        )),
        Or(bl, br) => Ok(Or(
            Box::new(typeinference_anf(ty, bl)?),
            Box::new(typeinference_anf(ty, br)?),
        )),
        Neg(bl) => Ok(Neg(Box::new(typeinference_anf(ty, bl)?))),
    }
}

pub fn typeinference_anfs<T: Clone, X: Clone>(
    ty: &T,
    anfs: &[grammar::AnfInferable<X>],
) -> Result<Vec<AnfTyped<X>>>
where
    AVarExt<X>: ξ<Inferable, Ext = Option<T>> + ξ<Typed, Ext = T>,
    AValExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
{
    anfs.iter().map(|a| typeinference_anf(ty, a)).collect()
}

fn ignored_type() -> ETy {
    ETy::EBool
}

fn ignored_stype() -> STy {
    STy::SBool
}

pub fn typeinference_eexpr(e: &grammar::EExprInferable) -> Result<EExprTyped> {
    use crate::grammar::EExpr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), Box::new(typeinference_anf(&ETy::EBool, a)?))),
        EPrj(_ty, i, a) => {
            // ignore types for now.
            Ok(EPrj(
                ignored_type(),
                *i,
                Box::new(typeinference_anf(&ETy::EBool, a)?),
            ))
        }
        EProd(_ty, anfs) => Ok(EProd(
            ignored_type(),
            typeinference_anfs(&ETy::EBool, anfs)?,
        )),
        ELetIn(_ty, s, ebound, ebody) => Ok(ELetIn(
            LetInTypes {
                bindee: ignored_type(),
                body: ignored_type(),
            },
            s.clone(),
            Box::new(typeinference_eexpr(ebound)?),
            Box::new(typeinference_eexpr(ebody)?),
        )),
        EIte(_ty, cond, t, f) => Ok(EIte(
            ignored_type(),
            Box::new(typeinference_anf(&ETy::EBool, cond)?),
            Box::new(typeinference_eexpr(t)?),
            Box::new(typeinference_eexpr(f)?),
        )),
        EFlip(_, param) => Ok(EFlip((), *param)),
        EObserve(_, a) => Ok(EObserve((), Box::new(typeinference_anf(&ETy::EBool, a)?))),
        ESample(_, e) => Ok(ESample((), Box::new(typeinference_eexpr(e)?))),
        ESample2(_, e) => Ok(ESample2((), Box::new(typeinference_sexpr(e)?))),
    }
}

pub fn typeinference_sexpr(e: &grammar::SExprInferable) -> Result<SExprTyped> {
    use crate::grammar::SExpr::*;
    match e {
        SAnf(_, a) => Ok(SAnf((), Box::new(typeinference_anf(&STy::SBool, a)?))),
        SLetIn(_ty, s, ebound, ebody) => Ok(SLetIn(
            LetInTypes {
                bindee: ignored_stype(),
                body: ignored_stype(),
            },
            s.clone(),
            Box::new(typeinference_sexpr(ebound)?),
            Box::new(typeinference_sexpr(ebody)?),
        )),
        SSeq((), e0, e1) => Ok(SSeq(
            (),
            Box::new(typeinference_sexpr(e0)?),
            Box::new(typeinference_sexpr(e1)?),
        )),
        SIte(_ty, cond, t, f) => Ok(SIte(
            ignored_stype(),
            Box::new(typeinference_anf(&STy::SBool, cond)?),
            Box::new(typeinference_sexpr(t)?),
            Box::new(typeinference_sexpr(f)?),
        )),
        SFlip(_, param) => Ok(SFlip((), *param)),
        SExact(_, e) => Ok(SExact((), Box::new(typeinference_eexpr(e)?))),
    }
}

pub fn typeinference(p: &grammar::ProgramInferable) -> Result<ProgramTyped> {
    match p {
        Program::EBody(e) => Ok(Program::EBody(typeinference_eexpr(e)?)),
        Program::SBody(e) => Ok(Program::SBody(typeinference_sexpr(e)?)),
    }
}

pub fn pipeline(p: &grammar::ProgramInferable) -> Result<ProgramTyped> {
    typeinference(p)
}
