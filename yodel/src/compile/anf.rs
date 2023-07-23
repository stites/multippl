use super::grammar::*;
use crate::annotate::grammar::*;
use crate::data::*;
use crate::desugar::exact::integers;
use crate::grammar::*;
use itertools::Itertools;
use itertools::*;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::builder::bdd_plan::BddPlan;

pub fn eval_sanf_var(ctx: &SCtx, d: &NamedVar, s: &str) -> Result<Vec<SVal>> {
    match ctx.substitutions.get(&d.id()) {
        None => Err(Generic(format!(
            "variable {} does not reference known substitution",
            s
        ))),
        Some(v) => Ok(v.clone()),
    }
}

pub fn eval_eanf_var(ctx: &ECtx, d: &NamedVar, s: &str) -> Result<Vec<EVal>> {
    match ctx.substitutions.get(&d.id()) {
        None => Err(Generic(format!(
            "variable {} does not reference known substitution",
            s
        ))),
        Some((v, _)) => Ok(v.clone()),
    }
}
pub fn eval_sanf_numop<'a>(
    ctx: &'a SCtx,
    bl: &'a AnfAnn<SVal>,
    br: &'a AnfAnn<SVal>,
    fop: impl Fn(f64, f64) -> f64,
    iop: impl Fn(u64, u64) -> u64,
    op: impl Fn(Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(SOutput, AnfTr<SVal>)> {
    let (ol, bl) = eval_sanf(ctx, &bl)?;
    let (or, br) = eval_sanf(ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([SVal::SFloat(l)], [SVal::SFloat(r)]) => {
            let out = ctx.as_output(vec![SVal::SFloat(fop(*l, *r))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([SVal::SInt(l)], [SVal::SInt(r)]) => {
            let out = ctx.as_output(vec![SVal::SInt(iop(*l, *r))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed(),
    }
}
pub fn eval_eanf_numop<'a>(
    ctx: &'a ECtx,
    bl: &'a AnfAnn<EVal>,
    br: &'a AnfAnn<EVal>,
    fop: impl Fn(f64, f64) -> f64,
    iop: impl Fn(usize, usize) -> usize,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<(EOutput, AnfTr<EVal>)> {
    let (ol, bl) = eval_eanf(ctx, &bl)?;
    let (or, br) = eval_eanf(ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([EVal::EFloat(l)], [EVal::EFloat(r)]) => {
            let out = ctx.as_output(vec![EVal::EFloat(fop(*l, *r))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([EVal::EProd(l)], [EVal::EProd(r)]) => {
            let l = integers::from_prod(l)?;
            let r = integers::from_prod(r)?;
            let out = ctx.as_output(vec![integers::as_onehot(iop(l, r))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([EVal::EInteger(l)], _) => return errors::erased(),
        (_, [EVal::EInteger(r)]) => return errors::erased(),
        _ => return errors::typecheck_failed(),
    }
}
pub fn eval_sanf_bop<'a>(
    ctx: &'a SCtx,
    bl: &'a AnfAnn<SVal>,
    br: &'a AnfAnn<SVal>,
    bop: impl Fn(bool, bool) -> bool,
    op: impl Fn(Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(SOutput, AnfTr<SVal>)> {
    let (ol, bl) = eval_sanf(ctx, &bl)?;
    let (or, br) = eval_sanf(ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([SVal::SBool(l)], [SVal::SBool(r)]) => {
            let out = ctx.as_output(vec![SVal::SBool(bop(*l, *r))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed(),
    }
}
pub fn eval_eanf_bop<'a>(
    ctx: &'a ECtx,
    bl: &'a AnfAnn<EVal>,
    br: &'a AnfAnn<EVal>,
    bop: impl Fn(Box<BddPlan>, Box<BddPlan>) -> BddPlan,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<(EOutput, AnfTr<EVal>)> {
    let (ol, bl) = eval_eanf(ctx, &bl)?;
    let (or, br) = eval_eanf(ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([EVal::EBdd(l)], [EVal::EBdd(r)]) => {
            let out = ctx.as_output(vec![EVal::EBdd(bop(
                Box::new(l.clone()),
                Box::new(r.clone()),
            ))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed(),
    }
}

pub fn eval_sanf_cop(
    ctx: &SCtx,
    bl: &AnfAnn<SVal>,
    br: &AnfAnn<SVal>,
    bop: impl Fn(&bool, &bool) -> bool,
    fop: impl Fn(&f64, &f64) -> bool,
    iop: impl Fn(&u64, &u64) -> bool,
    op: impl Fn(Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(SOutput, AnfTr<SVal>)> {
    let (ol, bl) = eval_sanf(ctx, &bl)?;
    let (or, br) = eval_sanf(ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([SVal::SBool(l)], [SVal::SBool(r)]) => {
            let out = ctx.as_output(vec![SVal::SBool(bop(l, r))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([SVal::SFloat(l)], [SVal::SFloat(r)]) => {
            let out = ctx.as_output(vec![SVal::SBool(fop(l, r))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([SVal::SInt(l)], [SVal::SInt(r)]) => {
            let out = ctx.as_output(vec![SVal::SBool(iop(l, r))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed(),
    }
}
pub fn eval_eanf_cop(
    ctx: &ECtx,
    bl: &AnfAnn<EVal>,
    br: &AnfAnn<EVal>,
    bop: Option<impl Fn(&BddPlan, &BddPlan) -> bool>,
    fop: impl Fn(&f64, &f64) -> bool,
    iop: impl Fn(&usize, &usize) -> bool,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<(EOutput, AnfTr<EVal>)> {
    let (ol, bl) = eval_eanf(ctx, &bl)?;
    let (or, br) = eval_eanf(ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([EVal::EBdd(l)], [EVal::EBdd(r)]) => match bop {
            None => return errors::typecheck_failed(),
            Some(bop) => {
                let out = ctx.as_output(vec![EVal::EBdd(BddPlan::from_bool(bop(l, r)))]);
                Ok((out.clone(), op(Box::new(bl), Box::new(br))))
            }
        },
        ([EVal::EFloat(l)], [EVal::EFloat(r)]) => {
            let out = ctx.as_output(vec![EVal::EBdd(BddPlan::from_bool(fop(l, r)))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([EVal::EInteger(l)], [EVal::EInteger(r)]) => {
            let out = ctx.as_output(vec![EVal::EBdd(BddPlan::from_bool(iop(l, r)))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed(),
    }
}
pub fn eval_sanf_vec(
    ctx: &SCtx,
    anfs: &[AnfAnn<SVal>],
    op: impl Fn(Vec<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(SOutput, AnfTr<SVal>)> {
    let (outs, trs): (Vec<SVal>, Vec<AnfTr<SVal>>) =
        anfs.iter().fold(Ok((vec![], vec![])), |acc, a| match acc {
            Ok((mut vs, mut trs)) => {
                let (aout, tr) = eval_sanf(ctx, a)?;
                trs.push(tr);
                if aout.out.len() > 1 {
                    return errors::generic("nested vector not (yet) supported");
                } else {
                    vs.extend(aout.out);
                }
                Ok((vs, trs))
            }
            Err(e) => Err(e),
        })?;
    let o = ctx.as_output(outs);
    Ok((o.clone(), op(trs)))
}
fn eval_sanf_dist2(
    ctx: &SCtx,
    sv: &SampledVar,
    p0: &AnfAnn<SVal>,
    p1: &AnfAnn<SVal>,
    mkdist: impl Fn(f64, f64) -> Dist,
    mkanf: impl Fn((SampledVar, SOutput), Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(SOutput, AnfTr<SVal>)> {
    let (o0, p0) = eval_sanf(ctx, p0)?;
    let (o1, p1) = eval_sanf(ctx, p1)?;
    match (&o0.out[..], &o1.out[..]) {
        ([SVal::SFloat(l)], [SVal::SFloat(r)]) => {
            let out = ctx.as_output(vec![SVal::SDist(mkdist(*l, *r))]);
            let ext = (sv.clone(), out.clone());
            Ok((out.clone(), mkanf(ext, Box::new(p0), Box::new(p1))))
        }
        _ => return errors::typecheck_failed(),
    }
}
fn vals2vec_params(vs: &[SVal]) -> Result<Vec<f64>> {
    vs.iter()
        .map(|v| match v {
            SVal::SFloat(f) => Ok(*f),
            _ => errors::typecheck_failed(),
        })
        .collect()
}
fn eval_sanf_dist_vec(
    ctx: &SCtx,
    sv: &SampledVar,
    ps: &[AnfAnn<SVal>],
    mkdist: impl Fn(Vec<f64>) -> Dist,
    mkanf: impl Fn((SampledVar, SOutput), Vec<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(SOutput, AnfTr<SVal>)> {
    let (o, ps) = eval_sanfs(ctx, ps)?;
    match &o.out[..] {
        [SVal::SVec(vs)] => {
            let vs = vals2vec_params(&vs)?;
            let out = ctx.as_output(vec![SVal::SDist(mkdist(vs))]);
            let ext = (sv.clone(), out.clone());
            Ok((out.clone(), mkanf(ext, ps)))
        }
        _ => return errors::typecheck_failed(),
    }
}
pub fn eval_sanf<'a>(ctx: &'a SCtx, a: &'a AnfAnn<SVal>) -> Result<(SOutput, AnfTr<SVal>)> {
    use Anf::*;
    match a {
        AVal(_, v) => {
            let out = ctx.as_output(vec![v.clone()]);
            Ok((out.clone(), AVal(Output::sample(out), v.clone())))
        }
        AVar(d, s) => {
            let v = eval_sanf_var(ctx, d, s)?;
            let out = ctx.as_output(v);
            Ok((out.clone(), AVar(out.pkg(), s.to_string())))
        }
        Neg(bl) => {
            let (ol, bl) = eval_sanf(ctx, &bl)?;
            match &ol.out[..] {
                [SVal::SBool(l)] => {
                    let out = ctx.as_output(vec![SVal::SBool(!*l)]);
                    Ok((out.clone(), Neg(Box::new(bl))))
                }
                _ => return errors::typecheck_failed(),
            }
        }
        Or(bl, br) => eval_sanf_bop(ctx, bl, br, |a, b| a || b, Or),
        And(bl, br) => eval_sanf_bop(ctx, bl, br, |a, b| a && b, And),

        Plus(bl, br) => eval_sanf_numop(ctx, bl, br, |a, b| a + b, |a, b| a + b, Plus),
        Minus(bl, br) => eval_sanf_numop(ctx, bl, br, |a, b| a - b, |a, b| a - b, Minus),
        Mult(bl, br) => eval_sanf_numop(ctx, bl, br, |a, b| a * b, |a, b| a * b, Mult),
        Div(bl, br) => eval_sanf_numop(ctx, bl, br, |a, b| a / b, |a, b| a / b, Div),

        GT(bl, br) => eval_sanf_cop(
            ctx,
            bl,
            br,
            PartialOrd::gt,
            PartialOrd::gt,
            PartialOrd::gt,
            GT,
        ),
        GTE(bl, br) => eval_sanf_cop(
            ctx,
            bl,
            br,
            PartialOrd::ge,
            PartialOrd::ge,
            PartialOrd::ge,
            GTE,
        ),
        LT(bl, br) => eval_sanf_cop(
            ctx,
            bl,
            br,
            PartialOrd::lt,
            PartialOrd::lt,
            PartialOrd::lt,
            LT,
        ),
        LTE(bl, br) => eval_sanf_cop(
            ctx,
            bl,
            br,
            PartialOrd::le,
            PartialOrd::le,
            PartialOrd::le,
            LTE,
        ),
        EQ(bl, br) => eval_sanf_cop(ctx, bl, br, PartialEq::eq, PartialEq::eq, PartialEq::eq, EQ),

        AnfProd(anfs) => eval_sanf_vec(ctx, anfs, AnfProd),
        AnfVec(anfs) => eval_sanf_vec(ctx, anfs, AnfVec),
        AnfPrj(var, ix) => {
            let (ovar, var) = eval_sanf(ctx, var)?;
            let (oix, ix) = eval_sanf(ctx, ix)?;
            match (&ovar.out[..], &oix.out[..]) {
                ([SVal::SProd(vs)], [SVal::SInt(i)]) => {
                    let out = ctx.as_output(vec![vs[*i as usize].clone()]);
                    Ok((out.clone(), AnfPrj(Box::new(var), Box::new(ix))))
                }
                ([SVal::SVec(vs)], [SVal::SInt(i)]) => {
                    let out = ctx.as_output(vec![vs[*i as usize].clone()]);
                    Ok((out.clone(), AnfPrj(Box::new(var), Box::new(ix))))
                }
                _ => return errors::typecheck_failed(),
            }
        }
        AnfBernoulli(sv, p) => {
            let (o, p) = eval_sanf(ctx, p)?;
            match &o.out[..] {
                [SVal::SFloat(f)] => {
                    let out = ctx.as_output(vec![SVal::SDist(Dist::Bern(*f))]);
                    let ext = (sv.clone(), out.clone());
                    Ok((out.clone(), AnfBernoulli(ext, Box::new(p))))
                }
                _ => return errors::typecheck_failed(),
            }
        }
        AnfPoisson(sv, p) => {
            let (o, p) = eval_sanf(ctx, p)?;
            match &o.out[..] {
                [SVal::SInt(i)] => {
                    let out = ctx.as_output(vec![SVal::SDist(Dist::Poisson(*i))]);
                    let ext = (sv.clone(), out.clone());
                    Ok((out.clone(), AnfPoisson(ext, Box::new(p))))
                }
                _ => return errors::typecheck_failed(),
            }
        }
        AnfUniform(sv, p0, p1) => eval_sanf_dist2(ctx, sv, p0, p1, Dist::Uniform, AnfUniform),
        AnfNormal(sv, p0, p1) => eval_sanf_dist2(ctx, sv, p0, p1, Dist::Normal, AnfNormal),
        AnfBeta(sv, p0, p1) => eval_sanf_dist2(ctx, sv, p0, p1, Dist::Beta, AnfBeta),
        AnfDiscrete(sv, ps) => eval_sanf_dist_vec(ctx, sv, ps, Dist::Discrete, AnfDiscrete),
        AnfDirichlet(sv, ps) => eval_sanf_dist_vec(ctx, sv, ps, Dist::Dirichlet, AnfDirichlet),
    }
}

pub fn eval_sanfs(ctx: &SCtx, anfs: &[AnfAnn<SVal>]) -> Result<(SOutput, Vec<AnfTr<SVal>>)> {
    let (svals, trs) = anfs.iter().fold(Ok((vec![], vec![])), |res, a| {
        let (svals_fin, mut trs_fin) = res?;
        let (o, atr) = eval_sanf(ctx, a)?;
        let svals = svals_fin.iter().chain(&o.out).cloned().collect_vec();
        trs_fin.push(atr);
        Ok((svals, trs_fin))
    })?;
    Ok((ctx.as_output(svals), trs))
}

pub fn eval_eanf<'a>(ctx: &'a ECtx, a: &'a AnfAnn<EVal>) -> Result<(EOutput, AnfTr<EVal>)> {
    use Anf::*;
    match a {
        AVal(_, v) => {
            let out = ctx.as_output(vec![v.clone()]);
            Ok((out.clone(), AVal(Output::exact(out), v.clone())))
        }
        AVar(d, s) => {
            let v = eval_eanf_var(ctx, d, s)?;
            let out = ctx.as_output(v);
            Ok((out.clone(), AVar(out.pkg(), s.to_string())))
        }
        Neg(bl) => {
            let (ol, bl) = eval_eanf(ctx, &bl)?;
            match &ol.out[..] {
                [EVal::EBdd(bdd)] => {
                    let out = ctx.as_output(vec![EVal::EBdd(BddPlan::neg(bdd.clone()))]);
                    Ok((out.clone(), Neg(Box::new(bl))))
                }
                _ => return errors::typecheck_failed(),
            }
        }
        Or(bl, br) => eval_eanf_bop(ctx, bl, br, BddPlan::Or, Or),
        And(bl, br) => eval_eanf_bop(ctx, bl, br, BddPlan::And, Or),

        Plus(bl, br) => eval_eanf_numop(ctx, bl, br, |a, b| a + b, |a, b| a + b, Plus),
        Minus(bl, br) => eval_eanf_numop(ctx, bl, br, |a, b| a - b, |a, b| a - b, Minus),
        Mult(bl, br) => eval_eanf_numop(ctx, bl, br, |a, b| a * b, |a, b| a * b, Mult),
        Div(bl, br) => eval_eanf_numop(ctx, bl, br, |a, b| a / b, |a, b| a / b, Div),

        GT(bl, br) => eval_eanf_cop(
            ctx,
            bl,
            br,
            None::<&dyn Fn(&BddPlan, &BddPlan) -> bool>,
            PartialOrd::gt,
            PartialOrd::gt,
            GT,
        ),
        GTE(bl, br) => eval_eanf_cop(
            ctx,
            bl,
            br,
            None::<&dyn Fn(&BddPlan, &BddPlan) -> bool>,
            PartialOrd::ge,
            PartialOrd::ge,
            GTE,
        ),
        LT(bl, br) => eval_eanf_cop(
            ctx,
            bl,
            br,
            None::<&dyn Fn(&BddPlan, &BddPlan) -> bool>,
            PartialOrd::lt,
            PartialOrd::lt,
            LT,
        ),
        LTE(bl, br) => eval_eanf_cop(
            ctx,
            bl,
            br,
            None::<&dyn Fn(&BddPlan, &BddPlan) -> bool>,
            PartialOrd::le,
            PartialOrd::le,
            LTE,
        ),
        EQ(bl, br) => eval_eanf_cop(
            ctx,
            bl,
            br,
            Some(PartialEq::eq),
            PartialEq::eq,
            PartialEq::eq,
            EQ,
        ),

        AnfProd(anfs) => {
            let (outs, trs): (Vec<EVal>, Vec<AnfTr<EVal>>) =
                anfs.iter().fold(Ok((vec![], vec![])), |acc, a| match acc {
                    Ok((mut vs, mut trs)) => {
                        let (aout, tr) = eval_eanf(ctx, a)?;
                        trs.push(tr);
                        if aout.out.len() > 1 {
                            return errors::generic("nested vector not (yet) supported");
                        } else {
                            vs.extend(aout.out);
                        }
                        Ok((vs, trs))
                    }
                    Err(e) => Err(e),
                })?;
            Ok((ctx.as_output(outs), AnfProd(trs)))
        }
        AnfPrj(var, ix) => {
            let (ovar, var) = eval_eanf(ctx, var)?;
            let (oix, ix) = eval_eanf(ctx, ix)?;
            match (&ovar.out[..], &oix.out[..]) {
                ([EVal::EProd(vs)], [EVal::EInteger(i)]) => {
                    let out = ctx.as_output(vec![vs[*i].clone()]);
                    Ok((out.clone(), AnfPrj(Box::new(var), Box::new(ix))))
                }
                _ => return errors::typecheck_failed(),
            }
        }
        AnfVec(anfs) => errors::not_in_exact(),
        AnfBernoulli(sv, p) => errors::not_in_exact(),
        AnfPoisson(sv, p) => errors::not_in_exact(),
        AnfUniform(sv, p0, p1) => errors::not_in_exact(),
        AnfNormal(sv, p0, p1) => errors::not_in_exact(),
        AnfBeta(sv, p0, p1) => errors::not_in_exact(),
        AnfDiscrete(sv, ps) => errors::not_in_exact(),
        AnfDirichlet(sv, ps) => errors::not_in_exact(),
    }
}

pub fn eval_eanfs(ctx: &ECtx, anfs: &[AnfAnn<EVal>]) -> Result<(EOutput, Vec<AnfTr<EVal>>)> {
    let (vals, trs) = anfs.iter().fold(Ok((vec![], vec![])), |res, a| {
        let (vals_fin, mut trs_fin) = res?;
        let (o, atr) = eval_eanf(ctx, a)?;
        let vals = vals_fin.iter().chain(&o.out).cloned().collect_vec();
        trs_fin.push(atr);
        Ok((vals, trs_fin))
    })?;
    Ok((ctx.as_output(vals), trs))
}
