use crate::data::errors::{
    CompileError::{self, SemanticsError},
    Result,
};
use crate::grammar::*;
use crate::typeinf::grammar::*;
use crate::*;
use rsdd::builder::bdd_plan::BddPlan;
// use crate::typecheck::grammar::{AnfTyped, EExprTyped, LetInTypes, ProgramTyped, SExprTyped};

impl AnfInferable<SVal> {
    pub fn strip_anf(&self) -> Result<AnfInferable<EVal>> {
        use Anf::*;
        match self {
            AVar(ext, s) => Ok(AVar(None, s.clone())),
            AVal(ext, SVal::SBool(b)) => Ok(AVal((), EVal::EBdd(BddPtr::from_bool(*b)))),
            AVal(_, _) => Err(SemanticsError("not in the natural embedding".to_string())),
            And(l, r) => Ok(And(Box::new(l.strip_anf()?), Box::new(r.strip_anf()?))),
            Or(l, r) => Ok(Or(Box::new(l.strip_anf()?), Box::new(r.strip_anf()?))),
            Neg(n) => Ok(Neg(Box::new(n.strip_anf()?))),
            x => todo!("{:?}", x),
        }
    }
}
impl SExprInferable {
    pub fn strip_samples1(&self) -> Result<EExprInferable> {
        use EExpr::*;
        use SExpr::*;
        match self {
            SAnf(x, a) => Ok(EAnf((), Box::new(a.strip_anf()?))),
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
                Box::new(p.strip_anf()?),
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
