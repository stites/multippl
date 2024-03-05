use crate::data::errors::{
    CompileError::{self, SemanticsError},
    Result,
};
use crate::grammar::*;
use crate::typeinf::grammar::*;
use crate::*;
use std::fmt::Debug;

use crate::typecheck::grammar::Typed;
use crate::typeinf::grammar::Inferable;

fn upcast_anf_binop<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    l: &grammar::AnfUD<X>,
    r: &grammar::AnfUD<X>,
    op: impl Fn(Box<AnfInferable<X>>, Box<AnfInferable<X>>) -> AnfInferable<X>,
) -> Result<AnfInferable<X>>
where
    AVarExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = Option<T>>,
    // APrjExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    AValExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    ADistExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
{
    Ok(op(Box::new(upcast_anf(l)?), Box::new(upcast_anf(r)?)))
}
fn upcast_anf_vec<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    xs: &[grammar::AnfUD<X>],
    op: impl Fn(Vec<AnfInferable<X>>) -> AnfInferable<X>,
) -> Result<AnfInferable<X>>
where
    AVarExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = Option<T>>,
    // APrjExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    AValExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    ADistExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
{
    Ok(op(xs
        .iter()
        .map(|a| upcast_anf(a))
        .collect::<Result<Vec<_>>>()?))
}
fn upcast_anf<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    a: &grammar::AnfUD<X>,
) -> Result<AnfInferable<X>>
where
    AVarExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = Option<T>>,
    // APrjExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    ADistExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    AValExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
{
    use crate::grammar::Anf::*;
    match a {
        AVar(_, s) => Ok(AVar(None, s.clone())),
        AVal(_, v) => Ok(AVal((), v.clone())),

        // Booleans
        And(l, r) => upcast_anf_binop(l, r, And),
        Or(l, r) => upcast_anf_binop(l, r, Or),
        Neg(bl) => Ok(Neg(Box::new(upcast_anf(bl)?))),

        // Numerics
        Plus(l, r) => upcast_anf_binop(l, r, Plus),
        Minus(l, r) => upcast_anf_binop(l, r, Minus),
        Mult(l, r) => upcast_anf_binop(l, r, Mult),
        Div(l, r) => upcast_anf_binop(l, r, Div),

        // Ord
        GT(l, r) => upcast_anf_binop(l, r, GT),
        LT(l, r) => upcast_anf_binop(l, r, LT),
        GTE(l, r) => upcast_anf_binop(l, r, GTE),
        LTE(l, r) => upcast_anf_binop(l, r, LTE),
        EQ(l, r) => upcast_anf_binop(l, r, EQ),

        // [x]; (l,r); x[0]
        AnfVec(xs) => upcast_anf_vec(xs, AnfVec),
        AnfPush(xs, x) => Ok(AnfPush(Box::new(upcast_anf(xs)?), Box::new(upcast_anf(x)?))),
        AnfHead(xs) => Ok(AnfHead(Box::new(upcast_anf(xs)?))),
        AnfTail(xs) => Ok(AnfTail(Box::new(upcast_anf(xs)?))),

        AnfProd(xs) => upcast_anf_vec(xs, AnfProd),
        AnfPrj(var, ix) => Ok(AnfPrj(
            //     (),
            Box::new(upcast_anf(var)?),
            // var.clone(),
            Box::new(upcast_anf(ix)?),
        )),

        // Distributions
        AnfBernoulli(_, x) => Ok(AnfBernoulli((), Box::new(upcast_anf(x)?))),
        AnfPoisson(_, x) => Ok(AnfPoisson((), Box::new(upcast_anf(x)?))),
        AnfUniform(_, l, r) => upcast_anf_binop(l, r, |l, r| AnfUniform((), l, r)),
        AnfNormal(_, l, r) => upcast_anf_binop(l, r, |l, r| AnfNormal((), l, r)),
        AnfBeta(_, l, r) => upcast_anf_binop(l, r, |l, r| AnfBeta((), l, r)),
        AnfDiscrete(_, xs) => upcast_anf_vec(xs, |xs| AnfDiscrete((), xs)),
        AnfDirichlet(_, xs) => upcast_anf_vec(xs, |xs| AnfDirichlet((), xs)),
    }
}

fn upcast_anfs<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    anfs: &[grammar::AnfUD<X>],
) -> Result<Vec<AnfInferable<X>>>
where
    AVarExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = Option<T>>,
    // APrjExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    ADistExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    AValExt<X>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
{
    anfs.iter().map(|a| upcast_anf(a)).collect()
}

fn ignored_etype() -> ETy {
    ETy::EBool
}

fn ignored_stype() -> STy {
    STy::SBool
}

pub fn upcast_eexpr(e: &grammar::EExprUD) -> Result<EExprInferable> {
    use crate::grammar::EExpr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), Box::new(upcast_anf(a)?))),
        ELetIn(_, s, ebound, ebody) => Ok(ELetIn(
            None,
            s.clone(),
            Box::new(upcast_eexpr(ebound)?),
            Box::new(upcast_eexpr(ebody)?),
        )),
        EIte(_, cond, t, f) => Ok(EIte(
            None,
            Box::new(upcast_anf(cond)?),
            Box::new(upcast_eexpr(t)?),
            Box::new(upcast_eexpr(f)?),
        )),
        EFlip(_, param) => Ok(EFlip((), Box::new(upcast_anf(param)?))),
        EObserve(_, a, e) => Ok(EObserve(
            (),
            Box::new(upcast_anf(a)?),
            Box::new(upcast_eexpr(e)?),
        )),
        ESample(_, e) => Ok(ESample((), Box::new(upcast_sexpr(e)?))),

        EApp(_, f, args) => Ok(EApp(None, f.clone(), upcast_anfs(args)?)),
        EDiscrete(_, args) => Ok(EDiscrete((), upcast_anfs(args)?)),
        EIterate(_, f, init, times) => Ok(EIterate(
            (),
            f.clone(),
            Box::new(upcast_anf(init)?),
            Box::new(upcast_anf(times)?),
        )),
    }
}

fn upcast_sexpr(e: &grammar::SExprUD) -> Result<SExprInferable> {
    use crate::grammar::SExpr::*;
    match e {
        SAnf(_, a) => Ok(SAnf((), Box::new(upcast_anf(a)?))),
        SLetIn(_, s, ebound, ebody) => Ok(SLetIn(
            None,
            s.clone(),
            Box::new(upcast_sexpr(ebound)?),
            Box::new(upcast_sexpr(ebody)?),
        )),
        SSeq((), e0, e1) => Ok(SSeq(
            (),
            Box::new(upcast_sexpr(e0)?),
            Box::new(upcast_sexpr(e1)?),
        )),
        SIte(_, cond, t, f) => Ok(SIte(
            None,
            Box::new(upcast_anf(cond)?),
            Box::new(upcast_sexpr(t)?),
            Box::new(upcast_sexpr(f)?),
        )),
        SMap(_, arg, map, xs) => Ok(SMap(
            (),
            arg.clone(),
            Box::new(upcast_sexpr(map)?),
            Box::new(upcast_anf(xs)?),
        )),
        SFold(_, init, accum, arg, fold, xs) => Ok(SFold(
            (),
            Box::new(upcast_anf(init)?),
            accum.clone(),
            arg.clone(),
            Box::new(upcast_sexpr(fold)?),
            Box::new(upcast_anf(xs)?),
        )),
        SWhile(_, guard, body) => Ok(SWhile(
            (),
            Box::new(upcast_anf(guard)?),
            Box::new(upcast_sexpr(body)?),
        )),

        SApp(_, f, args) => Ok(SApp((), f.clone(), upcast_anfs(args)?)),
        SLambda(_, args, body) => Ok(SLambda((), args.clone(), Box::new(upcast_sexpr(body)?))),

        SSample(_, dist) => Ok(SSample((), Box::new(upcast_sexpr(dist)?))),
        SObserve(_, val, dist, rst) => Ok(SObserve(
            (),
            Box::new(upcast_anf(val)?),
            Box::new(upcast_anf(dist)?),
            Box::new(upcast_sexpr(rst)?),
        )),

        // Multi-language boundary
        SExact(_, e) => Ok(SExact((), Box::new(upcast_eexpr(e)?))),

        // sugar: let x = ~(<sexpr>) in <sexpr>
        SLetSample(_, var, dist, rest) => Ok(SLetSample(
            (),
            var.clone(),
            Box::new(upcast_sexpr(dist)?),
            Box::new(upcast_sexpr(rest)?),
        )),
    }
}

fn upcast_fun<ExprI, ExprT, Val, T>(
    upcast_expr: impl Fn(&ExprI) -> Result<ExprT>,
    f: &Function<ExprI>,
) -> Result<Function<ExprT>>
where
    <ExprI as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprT as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprI as Lang>::Ty: PartialEq + Debug + Clone,
    <ExprT as Lang>::Ty: PartialEq + Debug + Clone,
    ExprI: PartialEq + Debug + Clone + Lang<Anf = Anf<UD, Val>, Ty = T>,
    ExprT: PartialEq + Debug + Clone + Lang<Anf = Anf<Inferable, Val>, Ty = T>,
    AVarExt<Val>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = Option<<ExprT as Lang>::Ty>>,
    // APrjExt<Val>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    ADistExt<Val>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    AValExt<Val>: ξ<UD, Ext = ()> + ξ<Inferable, Ext = ()>,
    Val: Debug + PartialEq + Clone,
{
    Ok(Function {
        name: f.name.clone(),
        arguments: upcast_anfs(&f.arguments)?,
        body: upcast_expr(&f.body)?,
        returnty: f.returnty.clone(),
    })
}

pub fn upcast(p: &grammar::Program<UD>) -> Result<ProgramInferable> {
    use Program::*;
    match p {
        EBody(e) => Ok(EBody(upcast_eexpr(e)?)),
        SBody(e) => Ok(SBody(upcast_sexpr(e)?)),
        EDefine(f, p) => Ok(EDefine(upcast_fun(upcast_eexpr, f)?, Box::new(upcast(p)?))),
        SDefine(f, p) => Ok(SDefine(upcast_fun(upcast_sexpr, f)?, Box::new(upcast(p)?))),
    }
}
