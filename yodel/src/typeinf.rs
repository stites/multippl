use crate::data::errors::{
    CompileError::{self, SemanticsError},
    Result,
};
use crate::grammar::*;
use crate::typecheck::grammar::{AnfTyped, EExprTyped, ProgramTyped, SExprTyped};
use std::fmt::Debug;

pub mod grammar {
    use super::*;
    use crate::grammar::*;

    ::ttg::phase!(pub struct Inferable: {
        AVarExt<EVal>:Option<ETy>,
        EPrjExt:Option<ETy>,
        EProdExt:Option<ETy>,
        ELetInExt: Option<LetInTypes<ETy>>,
        EIteExt: Option<ETy>,
        EAppExt: Option<ETy>,

        AVarExt<SVal>: Option<STy>,
        SLetInExt: Option<LetInTypes<STy>>,
        SIteExt: Option<STy>,
    });
    ::ttg::alias!(Inferable + (Program, EExpr, SExpr, Anf<Var>));
}

use crate::typecheck::grammar::Typed;
use crate::typeinf::grammar::Inferable;

fn typeinf_anf_binop<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    ty: &T,
    l: &grammar::AnfInferable<X>,
    r: &grammar::AnfInferable<X>,
    op: impl Fn(Box<AnfTyped<X>>, Box<AnfTyped<X>>) -> AnfTyped<X>,
) -> Result<AnfTyped<X>>
where
    AVarExt<X>: ξ<Inferable, Ext = Option<T>> + ξ<Typed, Ext = T>,
    // APrjExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    AValExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    ADistExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
{
    Ok(op(
        Box::new(typeinference_anf(ty, l)?),
        Box::new(typeinference_anf(ty, r)?),
    ))
}
fn typeinf_anf_vec<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    ty: &T,
    xs: &[grammar::AnfInferable<X>],
    op: impl Fn(Vec<AnfTyped<X>>) -> AnfTyped<X>,
) -> Result<AnfTyped<X>>
where
    AVarExt<X>: ξ<Inferable, Ext = Option<T>> + ξ<Typed, Ext = T>,
    // APrjExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    AValExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    ADistExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
{
    Ok(op(xs
        .iter()
        .map(|a| typeinference_anf(ty, a))
        .collect::<Result<Vec<AnfTyped<X>>>>()?))
}
fn typeinference_anf<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    ty: &T,
    a: &grammar::AnfInferable<X>,
) -> Result<AnfTyped<X>>
where
    AVarExt<X>: ξ<Inferable, Ext = Option<T>> + ξ<Typed, Ext = T>,
    // APrjExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    ADistExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    AValExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
{
    use crate::grammar::Anf::*;
    match a {
        AVar(_, s) => Ok(AVar(ty.clone(), s.clone())),
        AVal(_, v) => Ok(AVal((), v.clone())),

        // Booleans
        And(l, r) => typeinf_anf_binop(ty, l, r, And),
        Or(l, r) => typeinf_anf_binop(ty, l, r, Or),
        Neg(bl) => Ok(Neg(Box::new(typeinference_anf(ty, bl)?))),

        // Numerics
        Plus(l, r) => typeinf_anf_binop(ty, l, r, Plus),
        Minus(l, r) => typeinf_anf_binop(ty, l, r, Minus),
        Mult(l, r) => typeinf_anf_binop(ty, l, r, Mult),
        Div(l, r) => typeinf_anf_binop(ty, l, r, Div),

        // Ord
        GT(l, r) => typeinf_anf_binop(ty, l, r, GT),
        LT(l, r) => typeinf_anf_binop(ty, l, r, LT),
        GTE(l, r) => typeinf_anf_binop(ty, l, r, GTE),
        LTE(l, r) => typeinf_anf_binop(ty, l, r, LTE),
        EQ(l, r) => typeinf_anf_binop(ty, l, r, EQ),

        // [x]; (l,r); x[0]
        AnfVec(xs) => typeinf_anf_vec(ty, xs, AnfVec),
        AnfPush(xs, x) => Ok(AnfPush(
            Box::new(typeinference_anf(ty, xs)?),
            Box::new(typeinference_anf(ty, x)?),
        )),
        AnfTrace(xs, x) => Ok(AnfTrace(
            Box::new(typeinference_anf(ty, xs)?),
            Box::new(typeinference_anf(ty, x)?),
        )),
        AnfHead(xs) => Ok(AnfHead(Box::new(typeinference_anf(ty, xs)?))),
        AnfTail(xs) => Ok(AnfTail(Box::new(typeinference_anf(ty, xs)?))),

        AnfProd(xs) => typeinf_anf_vec(ty, xs, AnfProd),
        AnfPrj(var, ix) => Ok(AnfPrj(
            //     (),
            Box::new(typeinference_anf(ty, var)?),
            // var.clone(),
            Box::new(typeinference_anf(ty, ix)?),
        )),

        // Distributions
        AnfBernoulli(_, x) => Ok(AnfBernoulli((), Box::new(typeinference_anf(ty, x)?))),
        AnfPoisson(_, x) => Ok(AnfPoisson((), Box::new(typeinference_anf(ty, x)?))),
        AnfUniform(_, l, r) => typeinf_anf_binop(ty, l, r, |l, r| AnfUniform((), l, r)),
        AnfNormal(_, l, r) => typeinf_anf_binop(ty, l, r, |l, r| AnfNormal((), l, r)),
        AnfBeta(_, l, r) => typeinf_anf_binop(ty, l, r, |l, r| AnfBeta((), l, r)),
        AnfDiscrete(_, xs) => typeinf_anf_vec(ty, xs, |xs| AnfDiscrete((), xs)),
        AnfDirichlet(_, xs) => typeinf_anf_vec(ty, xs, |xs| AnfDirichlet((), xs)),
    }
}

fn typeinference_anfs<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    ty: &T,
    anfs: &[grammar::AnfInferable<X>],
) -> Result<Vec<AnfTyped<X>>>
where
    AVarExt<X>: ξ<Inferable, Ext = Option<T>> + ξ<Typed, Ext = T>,
    // APrjExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    ADistExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    AValExt<X>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
{
    anfs.iter().map(|a| typeinference_anf(ty, a)).collect()
}

fn ignored_etype() -> ETy {
    ETy::EBool
}

fn ignored_stype() -> STy {
    STy::SBool
}

fn typeinference_eexpr(e: &grammar::EExprInferable) -> Result<EExprTyped> {
    use crate::grammar::EExpr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), Box::new(typeinference_anf(&ETy::EBool, a)?))),
        // EPrj(_ty, i, a) => {
        //     // ignore types for now.
        //     Ok(EPrj(
        //         ignored_etype(),
        //         Box::new(typeinference_anf(&ignored_etype(), i)?),
        //         Box::new(typeinference_anf(&ignored_etype(), a)?),
        //     ))
        // }
        // EProd(_ty, anfs) => Ok(EProd(
        //     ignored_etype(),
        //     typeinference_anfs(&ignored_etype(), anfs)?,
        // )),
        ELetIn(_ty, s, ebound, ebody) => Ok(ELetIn(
            LetInTypes {
                bindee: ignored_etype(),
                body: ignored_etype(),
            },
            s.clone(),
            Box::new(typeinference_eexpr(ebound)?),
            Box::new(typeinference_eexpr(ebody)?),
        )),
        EIte(_ty, cond, t, f) => Ok(EIte(
            ignored_etype(),
            Box::new(typeinference_anf(&ETy::EBool, cond)?),
            Box::new(typeinference_eexpr(t)?),
            Box::new(typeinference_eexpr(f)?),
        )),
        EFlip(_, param) => Ok(EFlip(
            (),
            Box::new(typeinference_anf(&ignored_etype(), param)?),
        )),
        EObserve(_, a, rst) => Ok(EObserve(
            (),
            Box::new(typeinference_anf(&ignored_etype(), a)?),
            Box::new(typeinference_eexpr(rst)?),
        )),
        ESample(_, e) => Ok(ESample((), Box::new(typeinference_sexpr(e)?))),

        EApp(_, f, args) => Ok(EApp(
            (),
            f.clone(),
            typeinference_anfs(&ignored_etype(), args)?,
        )),
        EDiscrete(_, args) => Ok(EDiscrete((), typeinference_anfs(&ignored_etype(), args)?)),
        EIterate(_, f, init, times) => Ok(EIterate(
            (),
            f.clone(),
            Box::new(typeinference_anf(&ignored_etype(), init)?),
            Box::new(typeinference_anf(&ignored_etype(), times)?),
        )),
    }
}

fn typeinference_sexpr(e: &grammar::SExprInferable) -> Result<SExprTyped> {
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
        SMap(_, arg, map, xs) => Ok(SMap(
            (),
            arg.clone(),
            Box::new(typeinference_sexpr(map)?),
            Box::new(typeinference_anf(&ignored_stype(), xs)?),
        )),
        SFold(_, init, accum, arg, fold, xs) => Ok(SFold(
            (),
            Box::new(typeinference_anf(&ignored_stype(), init)?),
            accum.clone(),
            arg.clone(),
            Box::new(typeinference_sexpr(fold)?),
            Box::new(typeinference_anf(&ignored_stype(), xs)?),
        )),
        SWhile(_, guard, body) => Ok(SWhile(
            (),
            Box::new(typeinference_anf(&ignored_stype(), guard)?),
            Box::new(typeinference_sexpr(body)?),
        )),

        SApp(_, f, args) => Ok(SApp(
            (),
            f.clone(),
            typeinference_anfs(&ignored_stype(), args)?,
        )),
        SLambda(_, args, body) => Ok(SLambda(
            (),
            args.clone(),
            Box::new(typeinference_sexpr(body)?),
        )),

        SSample(_, dist) => Ok(SSample((), Box::new(typeinference_sexpr(dist)?))),
        SObserve(_, val, dist, rst) => Ok(SObserve(
            (),
            Box::new(typeinference_anf(&ignored_stype(), val)?),
            Box::new(typeinference_anf(&ignored_stype(), dist)?),
            Box::new(typeinference_sexpr(rst)?),
        )),

        // Multi-language boundary
        SExact(_, e) => Ok(SExact((), Box::new(typeinference_eexpr(e)?))),

        // sugar: let x = ~(<sexpr>) in <sexpr>
        SLetSample(_, var, dist, rest) => Ok(SLetSample(
            (),
            var.clone(),
            Box::new(typeinference_sexpr(dist)?),
            Box::new(typeinference_sexpr(rest)?),
        )),
    }
}

fn typeinference_fun<ExprI, ExprT, Val, T>(
    typeinference_expr: impl Fn(&ExprI) -> Result<ExprT>,
    ty: T,
    f: &Function<ExprI>,
) -> Result<Function<ExprT>>
where
    <ExprI as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprT as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprI as Lang>::Ty: PartialEq + Debug + Clone,
    <ExprT as Lang>::Ty: PartialEq + Debug + Clone,
    ExprI: PartialEq + Debug + Clone + Lang<Anf = Anf<Inferable, Val>, Ty = T>,
    ExprT: PartialEq + Debug + Clone + Lang<Anf = Anf<Typed, Val>, Ty = T>,
    AVarExt<Val>: ξ<Inferable, Ext = Option<T>> + ξ<Typed, Ext = <ExprT as Lang>::Ty>,
    // APrjExt<Val>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    ADistExt<Val>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    AValExt<Val>: ξ<Inferable, Ext = ()> + ξ<Typed, Ext = ()>,
    Val: Debug + PartialEq + Clone,
{
    Ok(Function {
        name: f.name.clone(),
        arguments: typeinference_anfs(&ty, &f.arguments)?,
        body: typeinference_expr(&f.body)?,
        returnty: f.returnty.clone(),
    })
}

pub fn typeinference(p: &grammar::ProgramInferable) -> Result<ProgramTyped> {
    use Program::*;
    match p {
        EBody(e) => Ok(EBody(typeinference_eexpr(e)?)),
        SBody(e) => Ok(SBody(typeinference_sexpr(e)?)),
        EDefine(f, p) => Ok(EDefine(
            typeinference_fun(typeinference_eexpr, ignored_etype(), f)?,
            Box::new(typeinference(p)?),
        )),
        SDefine(f, p) => Ok(SDefine(
            typeinference_fun(typeinference_sexpr, ignored_stype(), f)?,
            Box::new(typeinference(p)?),
        )),
    }
}

pub fn pipeline(p: &grammar::ProgramInferable) -> Result<ProgramTyped> {
    typeinference(p)
}
