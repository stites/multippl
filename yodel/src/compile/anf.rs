use super::grammar::*;
use crate::annotate::grammar::*;
use crate::data::*;
use crate::grammar::*;
use itertools::Itertools;
use itertools::*;
use rsdd::builder::bdd_builder::DDNNFPtr;

pub fn mk_sval(v: &SVal) -> SVal {
    match v {
        SVal::SBool(b) => SVal::SBool(*b),
        SVal::SFloat(b) => SVal::SFloat(*b),
        SVal::SFloatVec(b) => SVal::SFloatVec(b.clone()),
        SVal::SInt(b) => SVal::SInt(*b),
    }
}

pub fn mk_sout(ctx: &Context, v: &SVal) -> Output {
    let mut out = Output::from_anf_dists(ctx, vec![]);
    out.sout = Some(v.clone());
    out
}

pub fn eval_sanf<'a>(
    ctx: &'a Context,
    a: &'a AnfAnn<SVal>,
) -> Result<(
    Output,
    AnfTr<SVal>,
    &'a dyn Fn(Compiled, AnfTr<SVal>) -> SExprTr,
)> {
    use Anf::*;
    let (o, anf) = match a {
        AVal(_, v) => {
            let out = mk_sout(ctx, v);
            Ok((out.clone(), AVal(Box::new(out), v.clone())))
        }
        AVar(d, s) => match ctx.ssubstitutions.get(&d.id()) {
            None => Err(Generic(format!(
                "variable {} does not reference known substitution",
                s
            ))),
            Some(v) => {
                let out = mk_sout(ctx, v);
                Ok((out.clone(), AVar(Box::new(out), s.to_string())))
            }
        },
        And(bl, br) => {
            let (ol, bl, _) = eval_sanf(ctx, &bl)?;
            let (or, br, _) = eval_sanf(ctx, &br)?;
            assert_eq!(ol.sout.unwrap().as_type(), STy::SBool);
            assert_eq!(or.sout.unwrap().as_type(), STy::SBool);
            todo!()
        }
        Or(bl, br) => {
            todo!()
        }
        Neg(bp) => {
            todo!()
        }
    }?;
    Ok((o, anf, &move |c, a| SExpr::SAnf(Box::new(c), Box::new(a))))
}

/// actually just eval_eanf, but not about to change this until later
pub fn eval_anf<Val: Clone>(
    mgr: &mut Mgr,
    ctx: &Context,
    a: &AnfAnn<Val>,
    out: &dyn Fn(&mut Mgr, &Val) -> Result<(Output, AnfTr<Val>)>,
) -> Result<(Output, AnfTr<Val>)>
where
    AValExt<Val>: ξ<Annotated, Ext = ()> + ξ<Trace, Ext = Box<Output>>,
    AVarExt<Val>: ξ<Annotated, Ext = NamedVar> + ξ<Trace, Ext = Box<Output>>,
{
    use Anf::*;
    match a {
        AVal(_, v) => out(mgr, v),
        AVar(d, s) => match ctx.substitutions.get(&d.id()) {
            None => Err(Generic(format!(
                "variable {} does not reference known substitution",
                s
            ))),
            Some((subs, subvar)) => {
                let c = Output::from_anf_dists(ctx, subs.to_vec());
                Ok((c.clone(), AVar(Box::new(c), s.to_string())))
            }
        },
        And(bl, br) => {
            let (ltr, rtr, o) = eval_anf_binop(mgr, ctx, bl, br, &BddManager::and, out)?;
            Ok((o, And(Box::new(ltr), Box::new(rtr))))
        }
        Or(bl, br) => {
            let (ltr, rtr, o) = eval_anf_binop(mgr, ctx, bl, br, &BddManager::or, out)?;
            Ok((o, Or(Box::new(ltr), Box::new(rtr))))
        }
        Neg(bp) => {
            let (mut p, ptr) = eval_anf(mgr, ctx, bp, out)?;
            p.dists = p.dists.iter().map(BddPtr::neg).collect_vec();
            Ok((p, Neg(Box::new(ptr))))
        }
    }
}

pub fn eval_anf_binop<Val: Clone>(
    mgr: &mut Mgr,
    ctx: &Context,
    bl: &AnfAnn<Val>,
    br: &AnfAnn<Val>,
    op: &dyn Fn(&mut Mgr, BddPtr, BddPtr) -> BddPtr,
    out: &dyn Fn(&mut Mgr, &Val) -> Result<(Output, AnfTr<Val>)>,
) -> Result<(AnfTr<Val>, AnfTr<Val>, Output)>
where
    AValExt<Val>: ξ<Annotated, Ext = ()> + ξ<Trace, Ext = Box<Output>>,
    AVarExt<Val>: ξ<Annotated, Ext = NamedVar> + ξ<Trace, Ext = Box<Output>>,
{
    let (l, ltr) = eval_anf(mgr, ctx, bl, out)?;
    let (r, rtr) = eval_anf(mgr, ctx, br, out)?;

    if l.dists.len() != r.dists.len() {
        return Err(SemanticsError(format!(
            "impossible! compiled {} dists on the left and {} formulas on the right.",
            l.dists.len(),
            r.dists.len()
        )));
    } else {
        let dists = izip!(l.dists, r.dists)
            .map(|(l, r)| op(mgr, l, r))
            .collect_vec();

        let dists_len = dists.len();
        Ok((ltr, rtr, Output::from_anf_dists(ctx, dists)))
    }
}

pub fn eval_eanf<'a>(
    mgr: &'a mut Mgr,
    ctx: &'a Context,
    a: &'a AnfAnn<EVal>,
) -> Result<(
    Output,
    Anf<Trace, EVal>,
    &'a dyn Fn(Compiled, AnfTr<EVal>) -> EExprTr,
)> {
    let (o, anf) = eval_anf(mgr, ctx, a, &mut |_, v| match v {
        EVal::EBool(b) => {
            let c = Output::from_anf_dists(ctx, vec![BddPtr::from_bool(*b)]);
            Ok((c.clone(), Anf::AVal(Box::new(c), EVal::EBool(*b))))
        }
        EVal::EProd(b) => Err(CompileError::Todo()),
    })?;
    Ok((o, anf, &move |c, a| EExpr::EAnf(Box::new(c), Box::new(a))))
}
