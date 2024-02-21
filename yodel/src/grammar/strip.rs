use crate::data::errors::{
    CompileError::{self, SemanticsError},
    Result,
};
use crate::grammar::*;
use crate::typeinf::grammar::*;
use crate::*;
use rsdd::builder::bdd_plan::BddPlan;
// use crate::typecheck::grammar::{AnfTyped, EExprTyped, LetInTypes, ProgramTyped, SExprTyped};

use itertools::Either;
impl AnfInferable<SVal> {
    pub fn strip_anf(&self) -> Result<Either<AnfInferable<EVal>, EExprInferable>> {
        use Anf::*;
        use Either::*;
        match self {
            AVar(ext, s) => Ok(Left(AVar(None, s.clone()))),
            AVal(ext, SVal::SBool(b)) => Ok(Left(AVal((), EVal::EBdd(BddPtr::from_bool(*b))))),
            AVal(ext, SVal::SFloat(f)) => Ok(Left(AVal((), EVal::EFloat(*f)))),
            And(l, r) => Ok(Left(And(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),

            Or(l, r) => Ok(Left(Or(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            Neg(n) => Ok(Left(Neg(Box::new(n.strip_anf()?.left().unwrap())))),
            Mult(l, r) => Ok(Left(Mult(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            Plus(l, r) => Ok(Left(Plus(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            Minus(l, r) => Ok(Left(Minus(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            Div(l, r) => Ok(Left(Div(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            GT(l, r) => Ok(Left(GT(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            GTE(l, r) => Ok(Left(GTE(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            LT(l, r) => Ok(Left(LT(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            LTE(l, r) => Ok(Left(LTE(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            EQ(l, r) => Ok(Left(EQ(
                Box::new(l.strip_anf()?.left().unwrap()),
                Box::new(r.strip_anf()?.left().unwrap()),
            ))),
            AnfBernoulli((), p) => Ok(Right(EExpr::EFlip(
                (),
                Box::new(p.strip_anf()?.left().unwrap()),
            ))),
            AVal(ext, SVal::SDist(Dist::Bern(p))) => Ok(Right(EExpr::EFlip(
                (),
                Box::new(Anf::AVal((), EVal::EFloat(*p))),
            ))),
            AVal(_, _) => Err(SemanticsError("not in the natural embedding".to_string())),
            x => Err(SemanticsError(format!(
                "can't convert expression {:?} to the exact language",
                x
            ))),
        }
    }
}
impl SExprInferable {
    pub fn strip_samples1(&self) -> Result<EExprInferable> {
        use EExpr::*;
        use Either::*;
        use SExpr::*;
        match self {
            SAnf(x, a) => match a.strip_anf()? {
                Left(a) => Ok(EAnf((), Box::new(a))),
                Right(e) => Ok(e),
            },
            // SBern(_, param) => match *param.clone() {
            //     Anf::AVal(_, SVal::SFloat(f)) => Ok(EFlip((), f)),
            //     a => Err(SemanticsError(format!(
            //         "cannot strip Bern({:?}) to EFlip",
            //         a
            //     ))),
            // },
            SExact(_, e) => Ok(e.strip_samples1()?),
            SLetIn(ex, s, x, y) => Ok(ELetIn(
                None,
                s.clone(),
                Box::new(x.strip_samples1()?),
                Box::new(y.strip_samples1()?),
            )),
            SIte(ex, p, x, y) => Ok(EIte(
                None,
                Box::new(p.strip_anf()?.left().unwrap()),
                Box::new(x.strip_samples1()?),
                Box::new(y.strip_samples1()?),
            )),
            SSeq(ex, x, y) => Ok(ELetIn(
                None,
                String::from("_"),
                Box::new(x.strip_samples1()?),
                Box::new(y.strip_samples1()?),
            )),
            s => Err(SemanticsError(
                "can't convert sampling expression as part of exact language".to_string(),
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

impl ProgramInferable {
    pub fn strip_samples(&self) -> Result<ProgramInferable> {
        use Program::*;
        match self {
            SBody(e) => Ok(EBody(e.strip_samples1()?)),
            EBody(e) => Ok(EBody(e.strip_samples1()?)),
            EDefine(f, p) => Ok(EDefine(f.clone(), Box::new(p.strip_samples()?))),
            SDefine(f, p) => Ok(SDefine(f.clone(), Box::new(p.strip_samples()?))),
        }
    }
}
