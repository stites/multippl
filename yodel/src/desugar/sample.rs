#![allow(non_camel_case_types)]

use crate::data::errors;
use crate::typeinf::grammar::{EExprInferable, Inferable};
/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
use crate::*;

use crate::desugar::exact::desugar_eexpr;
use crate::grammar::program::Program;

use ::core::fmt;
use ::core::fmt::Debug;
use itertools::Itertools;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::string::String;

pub fn desugar_sexpr(e: &grammar::SExprUD) -> Result<SExprUD> {
    use crate::grammar::SExpr::*;
    match e {
        SAnf(_, _) => Ok(e.clone()),
        SLetIn(_, s, ebound, ebody) => Ok(SLetIn(
            (),
            s.clone(),
            Box::new(desugar_sexpr(ebound)?),
            Box::new(desugar_sexpr(ebody)?),
        )),
        SSeq(_, e0, e1) => Ok(SSeq(
            (),
            Box::new(desugar_sexpr(e0)?),
            Box::new(desugar_sexpr(e1)?),
        )),
        SIte(_, cond, t, f) => Ok(SIte(
            (),
            cond.clone(),
            Box::new(desugar_sexpr(t)?),
            Box::new(desugar_sexpr(f)?),
        )),
        SMap(_, arg, map, xs) => Ok(SMap(
            (),
            arg.clone(),
            Box::new(desugar_sexpr(map)?),
            xs.clone(),
        )),
        SFold(_, init, accum, arg, fold, xs) => Ok(SFold(
            (),
            init.clone(),
            accum.clone(),
            arg.clone(),
            Box::new(desugar_sexpr(fold)?),
            xs.clone(),
        )),
        SWhile(_, guard, body) => Ok(SWhile((), guard.clone(), Box::new(desugar_sexpr(body)?))),

        SApp(_, f, args) => Ok(e.clone()),
        SLambda(_, args, body) => Ok(SLambda((), args.clone(), Box::new(desugar_sexpr(body)?))),

        SSample(_, dist) => Ok(SSample((), Box::new(desugar_sexpr(dist)?))),
        SObserve(_, _, _) => Ok(e.clone()),

        SExact(_, e) => Ok(SExact((), Box::new(desugar_eexpr(e)?))),

        // gone!
        SLetSample(_, var, dist, rest) => Ok(SLetIn(
            (),
            var.clone(),
            Box::new(SSample((), Box::new(desugar_sexpr(dist)?))),
            Box::new(desugar_sexpr(rest)?),
        )),
    }
}
