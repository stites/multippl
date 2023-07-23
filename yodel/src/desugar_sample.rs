#![allow(non_camel_case_types)]

use crate::data::errors;
use crate::typeinf::grammar::{EExprInferable, Inferable};
/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
use crate::*;

use crate::grammar::program::Program;

use ::core::fmt;
use ::core::fmt::Debug;
use itertools::Itertools;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::string::String;

fn desugar_sample_eexpr(e: &grammar::EExprUD) -> Result<EExprUD> {
    use crate::grammar::EExpr::*;
    match e {
        ELetIn(_, s, ebound, ebody) => Ok(ELetIn(
            (),
            s.clone(),
            Box::new(desugar_sample_eexpr(ebound)?),
            Box::new(desugar_sample_eexpr(ebody)?),
        )),
        EIte(_, cond, t, f) => Ok(EIte(
            (),
            cond.clone(),
            Box::new(desugar_sample_eexpr(t)?),
            Box::new(desugar_sample_eexpr(f)?),
        )),
        ESample(_, e) => Ok(ESample((), Box::new(desugar_sample_sexpr(e)?))),
        _ => Ok(e.clone()),
    }
}

pub fn desugar_sample_sexpr(e: &grammar::SExprUD) -> Result<SExprUD> {
    use crate::grammar::SExpr::*;
    match e {
        SAnf(_, _) => Ok(e.clone()),
        SLetIn(_, s, ebound, ebody) => Ok(SLetIn(
            (),
            s.clone(),
            Box::new(desugar_sample_sexpr(ebound)?),
            Box::new(desugar_sample_sexpr(ebody)?),
        )),
        SSeq(_, e0, e1) => Ok(SSeq(
            (),
            Box::new(desugar_sample_sexpr(e0)?),
            Box::new(desugar_sample_sexpr(e1)?),
        )),
        SIte(_, cond, t, f) => Ok(SIte(
            (),
            cond.clone(),
            Box::new(desugar_sample_sexpr(t)?),
            Box::new(desugar_sample_sexpr(f)?),
        )),
        SMap(_, arg, map, xs) => Ok(SMap(
            (),
            arg.clone(),
            Box::new(desugar_sample_sexpr(map)?),
            xs.clone(),
        )),
        SFold(_, init, accum, arg, fold, xs) => Ok(SFold(
            (),
            init.clone(),
            accum.clone(),
            arg.clone(),
            Box::new(desugar_sample_sexpr(fold)?),
            xs.clone(),
        )),
        SWhile(_, guard, body) => Ok(SWhile(
            (),
            guard.clone(),
            Box::new(desugar_sample_sexpr(body)?),
        )),

        SApp(_, f, args) => Ok(e.clone()),
        SLambda(_, args, body) => Ok(SLambda(
            (),
            args.clone(),
            Box::new(desugar_sample_sexpr(body)?),
        )),

        SSample(_, _) => Ok(e.clone()),
        SObserve(_, _, _) => Ok(e.clone()),

        SExact(_, e) => Ok(SExact((), Box::new(desugar_sample_eexpr(e)?))),

        // gone!
        SLetSample(_, var, dist, rest) => Ok(SLetIn(
            (),
            var.clone(),
            Box::new(SSample((), dist.clone())),
            Box::new(desugar_sample_sexpr(rest)?),
        )),
    }
}

fn desugar_sample_fun<ExprIn, ExprOut, Val, T>(
    translate_expr: impl Fn(&ExprIn) -> Result<ExprOut>,
    f: &Function<ExprIn>,
) -> Result<Function<ExprOut>>
where
    <ExprIn as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprOut as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprIn as Lang>::Ty: PartialEq + Debug + Clone,
    <ExprOut as Lang>::Ty: PartialEq + Debug + Clone,
    ExprIn: PartialEq + Debug + Clone + Lang<Anf = Anf<UD, Val>, Ty = T>,
    ExprOut: PartialEq + Debug + Clone + Lang<Anf = Anf<UD, Val>, Ty = T>,
    AVarExt<Val>: ξ<UD, Ext = ()>,
    // APrjExt<Val>: ξ<UD, Ext = ()>,
    ADistExt<Val>: ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<UD, Ext = ()>,
    Val: Debug + PartialEq + Clone,
{
    Ok(Function {
        name: f.name.clone(),
        arguments: f.arguments.clone(),
        body: translate_expr(&f.body)?,
        returnty: f.returnty.clone(),
    })
}

pub fn desugar_sample(p: &grammar::ProgramUD) -> Result<ProgramUD> {
    use Program::*;
    match p {
        EBody(e) => Ok(EBody(desugar_sample_eexpr(e)?)),
        SBody(e) => Ok(SBody(desugar_sample_sexpr(e)?)),
        EDefine(f, p) => Ok(EDefine(
            desugar_sample_fun(desugar_sample_eexpr, f)?,
            Box::new(desugar_sample(p)?),
        )),
        SDefine(f, p) => Ok(SDefine(
            desugar_sample_fun(desugar_sample_sexpr, f)?,
            Box::new(desugar_sample(p)?),
        )),
    }
}
