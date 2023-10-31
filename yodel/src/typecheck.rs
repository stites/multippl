use self::grammar::*;
use crate::grammar::*;
use crate::ttg::*;
use crate::*;
use std::fmt::Debug;

pub mod grammar {
    use super::*;
    use crate::grammar::classes::IsTyped;

    ::ttg::phase!(pub struct Typed: {
        AVarExt<EVal>:ETy,
        EPrjExt:ETy,
        EProdExt:ETy,
        ELetInExt: LetInTypes<ETy>,
        EIteExt: ETy,

        AVarExt<SVal>: STy,
        ADistExt<SVal>: (),
        SLetInExt: LetInTypes<STy>,
        SIteExt: STy,
    });
    ::ttg::alias!(Typed + (Program, EExpr, SExpr, Anf<Var>));

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
                // EPrj(t, _, _) => t.clone(),
                // EProd(t, _) => t.clone(),
                ELetIn(t, _, _, _) => t.body.clone(),
                EIte(t, _, _, _) => t.clone(),
                EFlip(_, _) => ETy::EBool,
                EObserve(_, _, _) => ETy::EBool,
                ESample(_, e) => todo!("natural_embedding_s(e.as_type())"),
                _ => todo!(),
            }
        }
    }

    impl IsTyped<STy> for SExprTyped {
        fn is_prod(&self) -> bool {
            todo!()
        }
        fn as_type(&self) -> STy {
            todo!()
        }
    }
}

fn typecheck_anf_binop<Val>(
    l: &grammar::AnfTyped<Val>,
    r: &grammar::AnfTyped<Val>,
    op: impl Fn(Box<AnfUD<Val>>, Box<AnfUD<Val>>) -> AnfUD<Val>,
) -> Result<AnfUD<Val>>
where
    AVarExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    // APrjExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    ADistExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    <AVarExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    // <APrjExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <AValExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <ADistExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    Val: Debug + PartialEq + Clone,
{
    Ok(op(Box::new(typecheck_anf(l)?), Box::new(typecheck_anf(r)?)))
}
fn typecheck_anf_vec<Val>(
    xs: &[grammar::AnfTyped<Val>],
    op: impl Fn(Vec<AnfUD<Val>>) -> AnfUD<Val>,
) -> Result<AnfUD<Val>>
where
    AVarExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    // APrjExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    ADistExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    <AVarExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    // <APrjExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <AValExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <ADistExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    Val: Debug + PartialEq + Clone,
{
    Ok(op(xs
        .iter()
        .map(|a| typecheck_anf(a))
        .collect::<Result<Vec<AnfUD<Val>>>>()?))
}

pub fn typecheck_anf<Val: Debug + PartialEq + Clone>(
    a: &grammar::AnfTyped<Val>,
) -> Result<AnfUD<Val>>
where
    AVarExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    // APrjExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    ADistExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    <AVarExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <AValExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <ADistExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    // <APrjExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    Val: Debug + PartialEq + Clone,
{
    use crate::grammar::Anf::*;
    match a {
        AVar(_, s) => Ok(AVar((), s.clone())),
        AVal(_, v) => Ok(AVal((), v.clone())),

        // Booleans
        And(l, r) => typecheck_anf_binop(l, r, And),
        Or(l, r) => typecheck_anf_binop(l, r, Or),
        Neg(bl) => Ok(Neg(Box::new(typecheck_anf(bl)?))),

        // Numerics
        Plus(l, r) => typecheck_anf_binop(l, r, Plus),
        Minus(l, r) => typecheck_anf_binop(l, r, Minus),
        Mult(l, r) => typecheck_anf_binop(l, r, Mult),
        Div(l, r) => typecheck_anf_binop(l, r, Div),

        // Ord
        GT(l, r) => typecheck_anf_binop(l, r, GT),
        LT(l, r) => typecheck_anf_binop(l, r, LT),
        GTE(l, r) => typecheck_anf_binop(l, r, GTE),
        LTE(l, r) => typecheck_anf_binop(l, r, LTE),
        EQ(l, r) => typecheck_anf_binop(l, r, EQ),

        // [x]; (l,r); x[0]
        AnfVec(xs) => typecheck_anf_vec(xs, AnfVec),
        AnfProd(xs) => typecheck_anf_vec(xs, AnfProd),
        AnfPrj(var, ix) => Ok(AnfPrj(
            Box::new(typecheck_anf(var)?),
            Box::new(typecheck_anf(ix)?),
        )),

        // Distributions
        AnfBernoulli(_, x) => Ok(AnfBernoulli((), Box::new(typecheck_anf(x)?))),
        AnfPoisson(_, x) => Ok(AnfPoisson((), Box::new(typecheck_anf(x)?))),
        AnfUniform(_, l, r) => typecheck_anf_binop(l, r, |l, r| AnfUniform((), l, r)),
        AnfNormal(_, l, r) => typecheck_anf_binop(l, r, |l, r| AnfNormal((), l, r)),
        AnfBeta(_, l, r) => typecheck_anf_binop(l, r, |l, r| AnfBeta((), l, r)),
        AnfDiscrete(_, xs) => typecheck_anf_vec(xs, |xs| AnfDiscrete((), xs)),
        AnfDirichlet(_, xs) => typecheck_anf_vec(xs, |xs| AnfDirichlet((), xs)),
    }
}

pub fn typecheck_anfs<Val: Debug + PartialEq + Clone>(
    anfs: &[AnfTyped<Val>],
) -> Result<Vec<AnfUD<Val>>>
where
    AVarExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    // APrjExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    ADistExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<Typed> + ξ<UD, Ext = ()>,
    <AVarExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    // <APrjExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <ADistExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    <AValExt<Val> as ξ<Typed>>::Ext: Debug + PartialEq + Clone,
    Val: Debug + PartialEq + Clone,
{
    anfs.iter().map(typecheck_anf).collect()
}

fn typecheck_eexpr(e: &grammar::EExprTyped) -> Result<EExprUD> {
    use crate::grammar::EExpr::*;
    match e {
        EAnf(_, a) => Ok(EAnf((), Box::new(typecheck_anf(a)?))),
        // EPrj(_ty, i, a) => Ok(EPrj(
        //     (),
        //     Box::new(typecheck_anf(i)?),
        //     Box::new(typecheck_anf(a)?),
        // )),
        // EProd(_ty, anfs) => Ok(EProd((), typecheck_anfs(anfs)?)),
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
        EFlip(_, param) => Ok(EFlip((), Box::new(typecheck_anf(param)?))),
        EObserve(_, a, e) => Ok(EObserve(
            (),
            Box::new(typecheck_anf(a)?),
            Box::new(typecheck_eexpr(e)?),
        )),
        ESample(_, e) => Ok(ESample((), Box::new(typecheck_sexpr(e)?))),

        EApp(_, f, args) => Ok(EApp((), f.clone(), typecheck_anfs(args)?)),
        EDiscrete(_, args) => Ok(EDiscrete((), typecheck_anfs(args)?)),
        EIterate(_, f, init, times) => Ok(EIterate(
            (),
            f.clone(),
            Box::new(typecheck_anf(init)?),
            Box::new(typecheck_anf(times)?),
        )),
    }
}

fn typecheck_sexpr(e: &grammar::SExprTyped) -> Result<SExprUD> {
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
        SMap(_, arg, map, xs) => Ok(SMap(
            (),
            arg.clone(),
            Box::new(typecheck_sexpr(map)?),
            Box::new(typecheck_anf(xs)?),
        )),
        SFold(_, init, accum, arg, fold, xs) => Ok(SFold(
            (),
            Box::new(typecheck_anf(init)?),
            accum.clone(),
            arg.clone(),
            Box::new(typecheck_sexpr(fold)?),
            Box::new(typecheck_anf(xs)?),
        )),
        SWhile(_, guard, body) => Ok(SWhile(
            (),
            Box::new(typecheck_anf(guard)?),
            Box::new(typecheck_sexpr(body)?),
        )),

        SApp(_, f, args) => Ok(SApp((), f.clone(), typecheck_anfs(args)?)),
        SLambda(_, args, body) => Ok(SLambda((), args.clone(), Box::new(typecheck_sexpr(body)?))),

        SSample(_, dist) => Ok(SSample((), Box::new(typecheck_sexpr(dist)?))),
        SObserve(_, val, dist, e) => Ok(SObserve(
            (),
            Box::new(typecheck_anf(val)?),
            Box::new(typecheck_anf(dist)?),
            Box::new(typecheck_sexpr(e)?),
        )),

        // Multi-language boundary
        SExact(_, e) => Ok(SExact((), Box::new(typecheck_eexpr(e)?))),

        // sugar: let x = ~(<sexpr>) in <sexpr>
        SLetSample(_, var, dist, rest) => Ok(SLetSample(
            (),
            var.clone(),
            Box::new(typecheck_sexpr(dist)?),
            Box::new(typecheck_sexpr(rest)?),
        )),
    }
}

fn typecheck_fun<ExprIn, ExprOut, Val, T>(
    translate_expr: impl Fn(&ExprIn) -> Result<ExprOut>,
    f: &Function<ExprIn>,
) -> Result<Function<ExprOut>>
where
    <ExprIn as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprOut as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprIn as Lang>::Ty: PartialEq + Debug + Clone,
    <ExprOut as Lang>::Ty: PartialEq + Debug + Clone,
    ExprIn: PartialEq + Debug + Clone + Lang<Anf = Anf<Typed, Val>, Ty = T>,
    ExprOut: PartialEq + Debug + Clone + Lang<Anf = Anf<UD, Val>, Ty = T>,
    AVarExt<Val>: ξ<UD, Ext = ()> + ξ<Typed, Ext = <ExprOut as Lang>::Ty>,
    // APrjExt<Val>: ξ<UD, Ext = ()> + ξ<Typed, Ext = ()>,
    AValExt<Val>: ξ<UD, Ext = ()> + ξ<Typed, Ext = ()>,
    ADistExt<Val>: ξ<UD, Ext = ()> + ξ<Typed, Ext = ()>,
    Val: Debug + PartialEq + Clone,
{
    Ok(Function {
        name: f.name.clone(),
        arguments: typecheck_anfs(&f.arguments)?,
        body: translate_expr(&f.body)?,
        returnty: f.returnty.clone(),
    })
}

pub fn typecheck(p: &grammar::ProgramTyped) -> Result<ProgramUD> {
    use Program::*;
    match p {
        EBody(e) => Ok(EBody(typecheck_eexpr(e)?)),
        SBody(e) => Ok(SBody(typecheck_sexpr(e)?)),
        EDefine(f, p) => Ok(EDefine(
            typecheck_fun(typecheck_eexpr, f)?,
            Box::new(typecheck(p)?),
        )),
        SDefine(f, p) => Ok(SDefine(
            typecheck_fun(typecheck_sexpr, f)?,
            Box::new(typecheck(p)?),
        )),
    }
}

pub fn pipeline(p: &crate::typeinf::grammar::ProgramInferable) -> Result<ProgramUD> {
    typecheck(&crate::typeinf::pipeline(p)?)
}
