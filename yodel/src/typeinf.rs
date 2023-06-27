use crate::data::errors::{
    CompileError::{self, SemanticsError},
    Result,
};
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
    impl ξ<Inferable> for SObserveExt {
        type Ext = ();
    }
    impl ξ<Inferable> for ESampleExt {
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

    // types are precise and do not need to be inferred
    impl ξ<Inferable> for SBernExt {
        type Ext = ();
    }
    impl ξ<Inferable> for SDiscreteExt {
        type Ext = ();
    }
    impl ξ<Inferable> for SUniformExt {
        type Ext = ();
    }
    impl ξ<Inferable> for SNormalExt {
        type Ext = ();
    }
    impl ξ<Inferable> for SBetaExt {
        type Ext = ();
    }
    impl ξ<Inferable> for SDirichletExt {
        type Ext = ();
    }

    impl ξ<Inferable> for SExactExt {
        type Ext = ();
    }

    pub type AnfInferable<X> = Anf<Inferable, X>;
    pub type EExprInferable = EExpr<Inferable>;
    pub type SExprInferable = SExpr<Inferable>;
    pub type ProgramInferable = Program<Inferable>;

    impl AnfInferable<SVal> {
        pub fn strip_anf(&self) -> Result<AnfInferable<EVal>> {
            use Anf::*;
            match self {
                AVar(ext, s) => Ok(AVar(None, s.clone())),
                AVal(ext, SVal::SBool(b)) => Ok(AVal((), EVal::EBool(*b))),
                AVal(_, _) => Err(SemanticsError("not in the natural embedding".to_string())),
                And(l, r) => Ok(And(Box::new(l.strip_anf()?), Box::new(r.strip_anf()?))),
                Or(l, r) => Ok(Or(Box::new(l.strip_anf()?), Box::new(r.strip_anf()?))),
                Neg(n) => Ok(Neg(Box::new(n.strip_anf()?))),
                _ => todo!(),
            }
        }
    }

    impl ProgramInferable {
        pub fn strip_samples(&self) -> Result<ProgramInferable> {
            use Program::*;
            match self {
                SBody(e) => Ok(EBody(e.strip_samples1()?)),
                EBody(e) => {
                    // FIXME: this shouldn't be necessary and I think I already fixed the bug that causes this.
                    let mut cur = e.strip_samples1()?;
                    loop {
                        let nxt = cur.strip_samples1()?;

                        if nxt == cur {
                            return Ok(EBody(nxt));
                        } else {
                            cur = nxt;
                        }
                    }
                }
            }
        }
    }
    impl SExprInferable {
        pub fn strip_samples1(&self) -> Result<EExprInferable> {
            use EExpr::*;
            use SExpr::*;
            match self {
                SAnf(x, a) => Ok(EAnf(x.clone(), Box::new(a.strip_anf()?))),
                SBern(_, param) => match *param.clone() {
                    Anf::AVal(_, SVal::SFloat(f)) => Ok(EFlip((), f)),
                    a => Err(SemanticsError(format!(
                        "cannot strip Bern({:?}) to EFlip",
                        a
                    ))),
                },
                SDiscrete(_, ps) => todo!("can't convert discrete, need to produce ESugar"),
                SUniform(_, lo, hi) => Err(SemanticsError("can't convert uniform".to_string())),
                SNormal(_, mean, var) => Err(SemanticsError("can't convert normal".to_string())),
                SBeta(_, a, b) => Err(SemanticsError("can't convert beta".to_string())),
                SDirichlet(_, ps) => Err(SemanticsError("can't convert Dirichlet".to_string())),
                SExact(_, e) => Ok(e.strip_samples1()?),
                SLetIn(ex, s, x, y) => Ok(ELetIn(
                    None,
                    s.clone(),
                    Box::new(x.strip_samples1()?),
                    Box::new(y.strip_samples1()?),
                )),
                SIte(ex, p, x, y) => Ok(EIte(
                    None,
                    Box::new(p.strip_anf()?),
                    Box::new(x.strip_samples1()?),
                    Box::new(y.strip_samples1()?),
                )),
                SObserve(_, a, e) => Err(SemanticsError(
                    "can't convert observes from sampling language".to_string(),
                )),
                SSeq(ex, x, y) => Ok(ELetIn(
                    None,
                    String::from("_"),
                    Box::new(x.strip_samples1()?),
                    Box::new(y.strip_samples1()?),
                )),
            }
        }
    }
    impl EExprInferable {
        pub fn strip_samples1(&self) -> Result<EExprInferable> {
            use EExpr::*;
            match self {
                ESample(_, e) => Ok(e.strip_samples1()?),
                ELetIn(ex, s, x, y) => Ok(ELetIn(
                    ex.clone(),
                    s.clone(),
                    Box::new(x.strip_samples1()?),
                    Box::new(y.strip_samples1()?),
                )),
                EIte(ex, p, x, y) => Ok(EIte(
                    ex.clone(),
                    p.clone(),
                    Box::new(x.strip_samples1()?),
                    Box::new(y.strip_samples1()?),
                )),
                e => Ok(e.clone()),
            }
        }
    }
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
        _ => todo!(),
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
            typeinference_anfs(&ignored_type(), anfs)?,
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
        ESample(_, e) => Ok(ESample((), Box::new(typeinference_sexpr(e)?))),
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
            Box::new(typeinference_anf(&ignored_stype(), cond)?),
            Box::new(typeinference_sexpr(t)?),
            Box::new(typeinference_sexpr(f)?),
        )),

        SBern(_, param) => {
            let param = typeinference_anf(&ignored_stype(), param)?;
            Ok(SBern((), Box::new(param)))
        }
        SUniform(_, lo, hi) => {
            let lo = typeinference_anf(&ignored_stype(), lo)?;
            let hi = typeinference_anf(&ignored_stype(), hi)?;
            Ok(SUniform((), Box::new(lo), Box::new(hi)))
        }
        SNormal(_, mean, var) => {
            let mean = typeinference_anf(&ignored_stype(), mean)?;
            let var = typeinference_anf(&ignored_stype(), var)?;
            Ok(SNormal((), Box::new(mean), Box::new(var)))
        }
        SBeta(_, a, b) => {
            let a = typeinference_anf(&ignored_stype(), a)?;
            let b = typeinference_anf(&ignored_stype(), b)?;
            Ok(SBeta((), Box::new(a), Box::new(b)))
        }
        SDiscrete(_, ps) => {
            let ps = typeinference_anfs(&ignored_stype(), ps)?;
            Ok(SDiscrete((), ps))
        }
        SDirichlet(_, ps) => {
            let ps = typeinference_anfs(&ignored_stype(), ps)?;
            Ok(SDirichlet((), ps))
        }
        SObserve(_, a, e) => Ok(SObserve(
            (),
            Box::new(typeinference_anf(&ignored_stype(), a)?),
            Box::new(typeinference_sexpr(e)?),
        )),

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
