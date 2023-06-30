use crate::data::CompileError;
use crate::grammar::*;
use grammar::*;
use std::fmt::Debug;

pub mod grammar {
    use super::*;

    ttg::phase!(pub struct Typed: {
        AVarExt<EVal>:ETy,
        EPrjExt:ETy,
        EProdExt:ETy,
        ELetInExt: LetInTypes<ETy>,
        EIteExt: ETy,

        AVarExt<SVal>: STy,
        SLetInExt: LetInTypes<STy>,
        SIteExt: STy,
    });
    ttg::alias!(Typed + (Program, EExpr, SExpr, Anf<Var>));

    #[derive(Debug, PartialEq, Clone)]
    pub struct LetInTypes<T> {
        pub bindee: T,
        pub body: T,
    }
    impl IsTyped<STy> for AnfTyped<SVal> {
        fn as_type(&self) -> STy {
            use Anf::*;
            match self {
                AVar(t, _) => t.clone(),
                AVal(_, v) => v.as_type(),
                _ => STy::SBool,
            }
        }
        fn is_prod(&self) -> bool {
            todo!()
        }
    }
    impl IsTyped<ETy> for AnfTyped<EVal> {
        fn as_type(&self) -> ETy {
            use Anf::*;
            match self {
                AVar(t, _) => t.clone(),
                AVal(_, v) => v.as_type(),
                _ => ETy::EBool,
            }
        }
        fn is_prod(&self) -> bool {
            todo!()
        }
    }
    impl AnfTyped<EVal> {
        pub fn var(s: String) -> AnfTyped<EVal> {
            Anf::AVar(ETy::EBool, s)
        }
    }
    impl AnfTyped<SVal> {
        pub fn var(s: String) -> AnfTyped<SVal> {
            Anf::AVar(STy::SBool, s)
        }
    }
    impl IsTyped<ETy> for EExprTyped {
        fn is_prod(&self) -> bool {
            todo!()
        }
        fn as_type(&self) -> ETy {
            use EExpr::*;
            match self {
                EAnf(_, anf) => anf.as_type(),
                EPrj(t, _, _) => t.clone(),
                EProd(t, _) => t.clone(),
                ELetIn(t, _, _, _) => t.body.clone(),
                EIte(t, _, _, _) => t.clone(),
                EFlip(_, _) => ETy::EBool,
                EObserve(_, _) => ETy::EBool,
                ESample(_, e) => natural_embedding_s(e.as_type()),
            }
        }
    }

    pub fn natural_embedding_e(ty: ETy) -> STy {
        match ty {
            ETy::EBool => STy::SBool,
            _ => todo!("probably need to return a result"),
        }
    }

    pub fn natural_embedding_s(ty: STy) -> ETy {
        match ty {
            STy::SBool => ETy::EBool,
            // TODO: can also convert vec<bool> to discrete
            _ => todo!("probably need to return a result"),
        }
    }

    impl IsTyped<STy> for SExprTyped {
        fn is_prod(&self) -> bool {
            todo!()
        }
        fn as_type(&self) -> STy {
            use SExpr::*;
            match self {
                SAnf(_, anf) => anf.as_type(),
                SLetIn(t, _, _, _) => t.body.clone(),
                SSeq(t, e0, e1) => e1.as_type(),
                SIte(t, _, _, _) => t.clone(),
                SBern(_, param) => STy::SBool,
                SDiscrete(_, ps) => STy::SInt,
                SUniform(_, lo, hi) => STy::SFloat,
                SNormal(_, mean, var) => STy::SFloat,
                SBeta(_, a, b) => STy::SFloat,
                SDirichlet(_, ps) => STy::SVec(ps.iter().map(|_| STy::SFloat).collect()),
                SObserve(_, _, _) => STy::SBool, // returns Top, 2 is the next best thing
                SExact(_, e) => natural_embedding_e(e.as_type()),
            }
        }
    }
}

pub fn typecheck_anf<Val: Debug + PartialEq + Clone>(
    a: &grammar::AnfTyped<Val>,
) -> Result<AnfUD<Val>, CompileError>
where
    AVarExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    <AVarExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <AValExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    Val: Debug + PartialEq + Clone,
{
    use crate::grammar::Anf::*;
    match a {
        AVar(_, s) => Ok(AVar((), s.clone())),
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
        _ => todo!(),
    }
}

pub fn typecheck_anfs<Val: Debug + PartialEq + Clone>(
    anfs: &[AnfTyped<Val>],
) -> Result<Vec<AnfUD<Val>>, CompileError>
where
    AVarExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    <AVarExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <AValExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    Val: Debug + PartialEq + Clone,
{
    anfs.iter().map(typecheck_anf).collect()
}

pub fn typecheck_eexpr(e: &grammar::EExprTyped) -> Result<EExprUD, CompileError> {
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
            Box::new(typecheck_eexpr(ebound)?),
            Box::new(typecheck_eexpr(ebody)?),
        )),
        EIte(_ty, cond, t, f) => Ok(EIte(
            (),
            Box::new(typecheck_anf(cond)?),
            Box::new(typecheck_eexpr(t)?),
            Box::new(typecheck_eexpr(f)?),
        )),
        EFlip(_, param) => Ok(EFlip((), *param)),
        EObserve(_, a) => Ok(EObserve((), Box::new(typecheck_anf(a)?))),
        ESample(_, e) => Ok(ESample((), Box::new(typecheck_sexpr(e)?))),
    }
}

pub fn typecheck_sexpr(e: &grammar::SExprTyped) -> Result<SExprUD, CompileError> {
    use crate::grammar::SExpr::*;
    match e {
        SAnf(_, a) => Ok(SAnf((), Box::new(typecheck_anf(a)?))),
        SLetIn(_ty, s, ebound, ebody) => Ok(SLetIn(
            (),
            s.clone(),
            Box::new(typecheck_sexpr(ebound)?),
            Box::new(typecheck_sexpr(ebody)?),
        )),
        SSeq((), e0, e1) => Ok(SSeq(
            (),
            Box::new(typecheck_sexpr(e0)?),
            Box::new(typecheck_sexpr(e1)?),
        )),
        SIte(_ty, cond, t, f) => Ok(SIte(
            (),
            Box::new(typecheck_anf(cond)?),
            Box::new(typecheck_sexpr(t)?),
            Box::new(typecheck_sexpr(f)?),
        )),

        SBern(_, param) => {
            let param = typecheck_anf(param)?;
            Ok(SBern((), Box::new(param)))
        }
        SUniform(_, lo, hi) => {
            let lo = typecheck_anf(lo)?;
            let hi = typecheck_anf(hi)?;
            Ok(SUniform((), Box::new(lo), Box::new(hi)))
        }
        SNormal(_, mean, var) => {
            let mean = typecheck_anf(mean)?;
            let var = typecheck_anf(var)?;
            Ok(SNormal((), Box::new(mean), Box::new(var)))
        }
        SBeta(_, a, b) => {
            let a = typecheck_anf(a)?;
            let b = typecheck_anf(b)?;
            Ok(SBeta((), Box::new(a), Box::new(b)))
        }
        SDiscrete(_, ps) => {
            let ps = typecheck_anfs(ps)?;
            Ok(SDiscrete((), ps))
        }
        SDirichlet(_, ps) => {
            let ps = typecheck_anfs(ps)?;
            Ok(SDirichlet((), ps))
        }
        SObserve(_, a, e) => Ok(SObserve(
            (),
            Box::new(typecheck_anf(a)?),
            Box::new(typecheck_sexpr(e)?),
        )),

        SExact(_, e) => Ok(SExact((), Box::new(typecheck_eexpr(e)?))),
    }
}

pub fn typecheck(p: &grammar::ProgramTyped) -> Result<ProgramUD, CompileError> {
    match p {
        Program::EBody(e) => Ok(Program::EBody(typecheck_eexpr(e)?)),
        Program::SBody(e) => Ok(Program::SBody(typecheck_sexpr(e)?)),
    }
}

pub fn pipeline(p: &crate::typeinf::grammar::ProgramInferable) -> Result<ProgramUD, CompileError> {
    typecheck(&crate::typeinf::pipeline(p)?)
}
