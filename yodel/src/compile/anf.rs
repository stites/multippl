use super::grammar::*;
use crate::annotate::grammar::*;
use crate::data::*;
use crate::desugar::exact::integers;
use crate::grammar::*;
use crate::uniquify::grammar::*;
use itertools::Itertools;
use itertools::*;
use rsdd::builder::bdd_builder::DDNNFPtr;
use rsdd::builder::bdd_plan::BddPlan;
use tracing::*;

use super::eval::State;

pub fn eval_sanf_var(
    state: &mut super::eval::State,
    ctx: &Ctx,
    nv: &NamedVar,
    s: &str,
) -> Result<Output> {
    let sout = ctx.sample.as_output(vec![]);
    let mut out = ctx.mk_soutput(sout);

    match out.sample.substitutions.get(&nv.id()).map(Subst::val) {
        Some(vs) => {
            out.sample.out = vs;
            Ok(out)
        }
        None => match out.exact.substitutions.get(&nv.id()).map(Subst::val) {
            None => Err(Generic(format!(
                "variable {} does not reference known substitution in: s{:?}, e{:?}",
                nv.name, out.sample.substitutions, out.exact.substitutions
            ))),
            Some(vs) => match &vs[..] {
                [EVal::EBdd(dist)] => {
                    let sample = super::sample::exact2sample_bdd_eff(state, &mut out, dist);
                    out.sample.out.push(SVal::SBool(sample));
                    out.sample.substitutions.insert(
                        nv.id(),
                        Subst::mk(vec![SVal::SBool(sample)], Some(Var::Named(nv.clone()))),
                    );
                    Ok(out)
                }
                _ => {
                    let embedding = vs
                        .iter()
                        .map(SExpr::<Trace>::embed)
                        .collect::<Result<Vec<_>>>()?;
                    out.sample.out = embedding.clone();
                    out.sample
                        .substitutions
                        .insert(nv.id(), Subst::mk(embedding, Some(Var::Named(nv.clone()))));
                    Ok(out)
                }
            },
        },
    }
}

pub fn eval_eanf_var(
    state: &mut super::eval::State,
    ctx: &Ctx,
    nv: &NamedVar,
    s: &str,
) -> Result<EOutput> {
    let mut eout = ctx.exact.as_output(vec![]);

    match ctx.exact.substitutions.get(&nv.id()).map(Subst::val) {
        Some(vs) => {
            eout.out = vs;
            Ok(eout)
        }
        None => match ctx.sample.substitutions.get(&nv.id()).map(Subst::val) {
            None => Err(Generic(format!(
                "variable {} does not reference known substitution in: s{:?}, e{:?}",
                nv.name, ctx.sample.substitutions, ctx.exact.substitutions
            ))),
            Some(vs) => {
                let embedding = vs
                    .iter()
                    .map(EExpr::<Trace>::embed)
                    .collect::<Result<Vec<_>>>()?;
                eout.out = embedding.clone();
                eout.substitutions
                    .insert(nv.id(), Subst::mk(embedding, Some(Var::Named(nv.clone()))));
                Ok(eout)
            }
        },
    }
}

pub fn eval_sanf_numop<'a>(
    state: &mut super::eval::State,
    ctx: &'a Ctx,
    bl: &'a AnfAnn<SVal>,
    br: &'a AnfAnn<SVal>,
    fop: impl Fn(f64, f64) -> f64,
    iop: impl Fn(u64, u64) -> u64,
    op: impl Fn(Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(Output, AnfTr<SVal>)> {
    let (ol, bl) = eval_sanf(state, ctx, &bl)?;
    let (or, br) = eval_sanf(state, ctx, &br)?;
    match (&ol.sample.out[..], &or.sample.out[..]) {
        ([SVal::SFloat(l)], [SVal::SFloat(r)]) => {
            let out = ctx.sample.as_output(vec![SVal::SFloat(fop(*l, *r))]);
            let out = ctx.mk_soutput(out);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([SVal::SInt(l)], [SVal::SInt(r)]) => {
            let out = ctx.sample.as_output(vec![SVal::SInt(iop(*l, *r))]);
            let out = ctx.mk_soutput(out);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed("sanf numop"),
    }
}
pub fn eval_eanf_numop<'a>(
    state: &mut super::eval::State,
    ctx: &'a Ctx,
    bl: &'a AnfAnn<EVal>,
    br: &'a AnfAnn<EVal>,
    fop: impl Fn(f64, f64) -> f64,
    iop: impl Fn(usize, usize) -> usize,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<(EOutput, AnfTr<EVal>)> {
    let span = tracing::span!(Level::DEBUG, "numop");
    let _enter = span.enter();

    let (ol, bl) = eval_eanf(state, ctx, &bl)?;
    let (or, br) = eval_eanf(state, ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([EVal::EFloat(l)], [EVal::EFloat(r)]) => {
            let x = fop(*l, *r);
            tracing::debug!("{l} <fop> {r} = {x}");
            let out = ctx.exact.as_output(vec![EVal::EFloat(x)]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([EVal::EInteger(l)], [EVal::EInteger(r)]) => {
            // let l = integers::from_prod(l)?;
            // let r = integers::from_prod(r)?;
            let x = iop(*l, *r);
            tracing::debug!("{l} <iop> {r} = {x}");
            // let out = ctx.exact.as_output(vec![integers::as_onehot(x)]);
            let out = ctx.exact.as_output(vec![EVal::EInteger(x)]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        // ([EVal::EInteger(l)], _) => return errors::erased(),
        // (_, [EVal::EInteger(r)]) => return errors::erased(),
        (l, r) => {
            return errors::typecheck_failed(&format!(
                "eanf numop. Arguments:\nleft: {:?}\nright: {:?}",
                l, r
            ))
        }
    }
}
pub fn eval_sanf_bop<'a>(
    state: &mut super::eval::State,
    ctx: &'a Ctx,
    bl: &'a AnfAnn<SVal>,
    br: &'a AnfAnn<SVal>,
    bop: impl Fn(bool, bool) -> bool,
    op: impl Fn(Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(Output, AnfTr<SVal>)> {
    let (ol, bl) = eval_sanf(state, ctx, &bl)?;
    let (or, br) = eval_sanf(state, ctx, &br)?;
    match (&ol.sample.out[..], &or.sample.out[..]) {
        ([SVal::SBool(l)], [SVal::SBool(r)]) => {
            let out = ctx.sample.as_output(vec![SVal::SBool(bop(*l, *r))]);
            let out = ctx.mk_soutput(out);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        (l, r) => {
            return errors::typecheck_failed(&format!(
                "sanf boolop. Arguments:\nleft: {:?}\nright: {:?}",
                l, r
            ))
        }
    }
}
pub fn eval_eanf_bop(
    state: &mut super::eval::State,
    ctx: &Ctx,
    bl: &AnfAnn<EVal>,
    br: &AnfAnn<EVal>,
    mut bop: impl FnMut(&mut Mgr, Box<BddPtr>, Box<BddPtr>) -> BddPtr,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<(EOutput, AnfTr<EVal>)> {
    let span = tracing::span!(Level::DEBUG, "binop");
    let _enter = span.enter();
    tracing::debug!("binop");

    let (ol, bl) = eval_eanf(state, ctx, &bl)?;
    let (or, br) = eval_eanf(state, ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([EVal::EBdd(l)], [EVal::EBdd(r)]) => {
            let out = ctx.exact.as_output(vec![EVal::EBdd(bop(
                &mut state.mgr,
                Box::new(l.clone()),
                Box::new(r.clone()),
            ))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed("eanf boolop"),
    }
}

pub fn eval_sanf_cop(
    state: &mut super::eval::State,
    ctx: &Ctx,
    bl: &AnfAnn<SVal>,
    br: &AnfAnn<SVal>,
    bop: impl Fn(&bool, &bool) -> bool,
    fop: impl Fn(&f64, &f64) -> bool,
    iop: impl Fn(&u64, &u64) -> bool,
    op: impl Fn(Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(Output, AnfTr<SVal>)> {
    let (ol, bl) = eval_sanf(state, ctx, &bl)?;
    let (or, br) = eval_sanf(state, ctx, &br)?;
    match (&ol.sample.out[..], &or.sample.out[..]) {
        ([SVal::SBool(l)], [SVal::SBool(r)]) => {
            let out = ctx.sample.as_output(vec![SVal::SBool(bop(l, r))]);
            let out = ctx.mk_soutput(out);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([SVal::SFloat(l)], [SVal::SFloat(r)]) => {
            let out = ctx.sample.as_output(vec![SVal::SBool(fop(l, r))]);
            let out = ctx.mk_soutput(out);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([SVal::SInt(l)], [SVal::SInt(r)]) => {
            let out = ctx.sample.as_output(vec![SVal::SBool(iop(l, r))]);
            let out = ctx.mk_soutput(out);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed("sanf compare op"),
    }
}
pub fn eval_eanf_cop(
    state: &mut super::eval::State,
    ctx: &Ctx,
    bl: &AnfAnn<EVal>,
    br: &AnfAnn<EVal>,
    bop: Option<impl Fn(&BddPtr, &BddPtr) -> bool>,
    fop: impl Fn(&f64, &f64) -> bool,
    iop: impl Fn(&usize, &usize) -> bool,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<(EOutput, AnfTr<EVal>)> {
    let (ol, bl) = eval_eanf(state, ctx, &bl)?;
    let (or, br) = eval_eanf(state, ctx, &br)?;
    match (&ol.out[..], &or.out[..]) {
        ([EVal::EBdd(l)], [EVal::EBdd(r)]) => match bop {
            None => return errors::typecheck_failed("eanf compare op invalid"),
            Some(bop) => {
                let out = ctx
                    .exact
                    .as_output(vec![EVal::EBdd(BddPtr::from_bool(bop(l, r)))]);
                Ok((out.clone(), op(Box::new(bl), Box::new(br))))
            }
        },
        ([EVal::EFloat(l)], [EVal::EFloat(r)]) => {
            let out = ctx
                .exact
                .as_output(vec![EVal::EBdd(BddPtr::from_bool(fop(l, r)))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        ([EVal::EInteger(l)], [EVal::EInteger(r)]) => {
            let out = ctx
                .exact
                .as_output(vec![EVal::EBdd(BddPtr::from_bool(iop(l, r)))]);
            Ok((out.clone(), op(Box::new(bl), Box::new(br))))
        }
        _ => return errors::typecheck_failed("eanf compare op"),
    }
}
pub fn eval_sanf_vec(
    state: &mut super::eval::State,
    ctx: &Ctx,
    anfs: &[AnfAnn<SVal>],
    op: impl Fn(Vec<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(Output, AnfTr<SVal>)> {
    let (outs, trs): (Vec<SVal>, Vec<AnfTr<SVal>>) =
        anfs.iter().fold(Ok((vec![], vec![])), |acc, a| match acc {
            Ok((mut vs, mut trs)) => {
                let (aout, tr) = eval_sanf(state, ctx, a)?;
                trs.push(tr);
                if aout.sample.out.len() > 1 {
                    return errors::generic("nested vector not (yet) supported");
                } else {
                    vs.extend(aout.sample.out);
                }
                Ok((vs, trs))
            }
            Err(e) => Err(e),
        })?;
    tracing::debug!("sanf_vec: {outs:?}");
    let o = ctx.sample.as_output(vec![SVal::SVec(outs)]);
    let o = ctx.mk_soutput(o);
    Ok((o.clone(), op(trs)))
}
fn eval_sanf_dist2(
    state: &mut super::eval::State,
    ctx: &Ctx,
    sv: &SampledVar,
    p0: &AnfAnn<SVal>,
    p1: &AnfAnn<SVal>,
    mkdist: impl Fn(f64, f64) -> Dist,
    mkanf: impl Fn((SampledVar, SOutput), Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(Output, AnfTr<SVal>)> {
    let (o0, p0) = eval_sanf(state, ctx, p0)?;
    let (o1, p1) = eval_sanf(state, ctx, p1)?;
    match (&o0.sample.out[..], &o1.sample.out[..]) {
        ([SVal::SFloat(l)], [SVal::SFloat(r)]) => {
            let out = ctx.sample.as_output(vec![SVal::SDist(mkdist(*l, *r))]);

            let out = ctx.mk_soutput(out);
            let ext = (sv.clone(), out.sample.clone());
            Ok((out.clone(), mkanf(ext, Box::new(p0), Box::new(p1))))
        }
        (l, r) => {
            tracing::debug!(" left: {l:?}");
            tracing::debug!("right: {r:?}");

            return errors::typecheck_failed("sanf dist with 2 args not given correct arguments.");
        }
    }
}
fn vals2vec_params(vs: &[SVal]) -> Result<Vec<f64>> {
    vs.iter()
        .map(|v| match v {
            SVal::SFloat(f) => Ok(*f),
            _ => errors::typecheck_failed("vec of vals is not floats"),
        })
        .collect()
}
fn eval_sanf_dist_vec(
    state: &mut super::eval::State,
    ctx: &Ctx,
    sv: &SampledVar,
    ps: &[AnfAnn<SVal>],
    mkdist: impl Fn(Vec<f64>) -> Dist,
    mkanf: impl Fn((SampledVar, SOutput), Vec<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<(Output, AnfTr<SVal>)> {
    let (o, ps) = eval_sanfs(state, ctx, ps)?;
    match &o.sample.out[..] {
        [SVal::SVec(vs)] => {
            let vs = vals2vec_params(&vs)?;
            let out = ctx.sample.as_output(vec![SVal::SDist(mkdist(vs))]);
            let out = ctx.mk_soutput(out);
            let ext = (sv.clone(), out.sample.clone());
            Ok((out.clone(), mkanf(ext, ps)))
        }
        _ => return errors::typecheck_failed("sanf distribution was not vector args not given"),
    }
}
pub fn eval_sanf<'a>(
    state: &'a mut super::eval::State,
    ctx: &'a Ctx,
    a: &'a AnfAnn<SVal>,
) -> Result<(Output, AnfTr<SVal>)> {
    use Anf::*;
    let span = tracing::span!(tracing::Level::DEBUG, "eval_sanf");
    let _enter = span.enter();
    match a {
        AVal(_, v) => {
            tracing::debug!("aval in: {v:?}");
            let out = ctx.sample.as_output(vec![v.clone()]);
            let out = ctx.mk_soutput(out);
            tracing::debug!("aval ot: {out:?}");
            Ok((out.clone(), AVal(out, v.clone())))
        }
        AVar(d, s) => {
            tracing::debug!("avar: {d:?} -- {s:?}");
            let out = eval_sanf_var(state, ctx, d, s)?;
            Ok((out.clone(), AVar(out, s.to_string())))
        }
        Neg(bl) => {
            let (ol, bl) = eval_sanf(state, ctx, &bl)?;
            match &ol.sample.out[..] {
                [SVal::SBool(l)] => {
                    let out = ctx.sample.as_output(vec![SVal::SBool(!*l)]);
                    let out = ctx.mk_soutput(out);
                    Ok((out.clone(), Neg(Box::new(bl))))
                }
                a => errors::typecheck_failed(&format!("sanf negation got {a:?}")),
            }
        }
        Or(bl, br) => eval_sanf_bop(state, ctx, bl, br, |a, b| a || b, Or),
        And(bl, br) => eval_sanf_bop(state, ctx, bl, br, |a, b| a && b, And),

        Plus(bl, br) => eval_sanf_numop(state, ctx, bl, br, |a, b| a + b, |a, b| a + b, Plus),
        Minus(bl, br) => eval_sanf_numop(state, ctx, bl, br, |a, b| a - b, |a, b| a - b, Minus),
        Mult(bl, br) => eval_sanf_numop(state, ctx, bl, br, |a, b| a * b, |a, b| a * b, Mult),
        Div(bl, br) => eval_sanf_numop(state, ctx, bl, br, |a, b| a / b, |a, b| a / b, Div),

        GT(bl, br) => eval_sanf_cop(
            state,
            ctx,
            bl,
            br,
            PartialOrd::gt,
            PartialOrd::gt,
            PartialOrd::gt,
            GT,
        ),
        GTE(bl, br) => eval_sanf_cop(
            state,
            ctx,
            bl,
            br,
            PartialOrd::ge,
            PartialOrd::ge,
            PartialOrd::ge,
            GTE,
        ),
        LT(bl, br) => eval_sanf_cop(
            state,
            ctx,
            bl,
            br,
            PartialOrd::lt,
            PartialOrd::lt,
            PartialOrd::lt,
            LT,
        ),
        LTE(bl, br) => eval_sanf_cop(
            state,
            ctx,
            bl,
            br,
            PartialOrd::le,
            PartialOrd::le,
            PartialOrd::le,
            LTE,
        ),
        EQ(bl, br) => eval_sanf_cop(
            state,
            ctx,
            bl,
            br,
            PartialEq::eq,
            PartialEq::eq,
            PartialEq::eq,
            EQ,
        ),

        AnfProd(anfs) => eval_sanf_vec(state, ctx, anfs, AnfProd),
        AnfVec(anfs) => eval_sanf_vec(state, ctx, anfs, AnfVec),
        AnfPrj(var, ix) => {
            let (ovar, var) = eval_sanf(state, ctx, var)?;
            let (oix, ix) = eval_sanf(state, ctx, ix)?;
            match (&ovar.sample.out[..], &oix.sample.out[..]) {
                ([SVal::SProd(vs)], [SVal::SInt(i)]) => {
                    let out = ctx.sample.as_output(vec![vs[*i as usize].clone()]);
                    let out = ctx.mk_soutput(out);
                    Ok((out.clone(), AnfPrj(Box::new(var), Box::new(ix))))
                }
                ([SVal::SVec(vs)], [SVal::SInt(i)]) => {
                    let out = ctx.sample.as_output(vec![vs[*i as usize].clone()]);
                    let out = ctx.mk_soutput(out);
                    Ok((out.clone(), AnfPrj(Box::new(var), Box::new(ix))))
                }
                a => return errors::typecheck_failed(&format!("anf projection got {a:?}")),
            }
        }
        AnfBernoulli(sv, p) => {
            let (o, p) = eval_sanf(state, ctx, p)?;
            match &o.sample.out[..] {
                [SVal::SFloat(f)] => {
                    let out = ctx.sample.as_output(vec![SVal::SDist(Dist::Bern(*f))]);
                    let out = ctx.mk_soutput(out);
                    let ext = (sv.clone(), out.sample.clone());
                    Ok((out.clone(), AnfBernoulli(ext, Box::new(p))))
                }
                a => return errors::typecheck_failed(&format!("anf bernoulli got {a:?}")),
            }
        }
        AnfPoisson(sv, p) => {
            let (o, p) = eval_sanf(state, ctx, p)?;
            match &o.sample.out[..] {
                [SVal::SInt(i)] => {
                    let out = ctx
                        .sample
                        .as_output(vec![SVal::SDist(Dist::Poisson(*i as f64))]);
                    let out = ctx.mk_soutput(out);
                    let ext = (sv.clone(), out.sample.clone());
                    Ok((out.clone(), AnfPoisson(ext, Box::new(p))))
                }
                [SVal::SFloat(f)] => {
                    let out = ctx.sample.as_output(vec![SVal::SDist(Dist::Poisson(*f))]);
                    let out = ctx.mk_soutput(out);
                    let ext = (sv.clone(), out.sample.clone());
                    Ok((out.clone(), AnfPoisson(ext, Box::new(p))))
                }
                a => return errors::typecheck_failed(&format!("anf poisson got {a:?}")),
            }
        }
        AnfUniform(sv, p0, p1) => {
            eval_sanf_dist2(state, ctx, sv, p0, p1, Dist::Uniform, AnfUniform)
        }
        AnfNormal(sv, p0, p1) => eval_sanf_dist2(state, ctx, sv, p0, p1, Dist::Normal, AnfNormal),
        AnfBeta(sv, p0, p1) => eval_sanf_dist2(state, ctx, sv, p0, p1, Dist::Beta, AnfBeta),
        AnfDiscrete(sv, ps) => eval_sanf_dist_vec(state, ctx, sv, ps, Dist::Discrete, AnfDiscrete),
        AnfDirichlet(sv, ps) => {
            eval_sanf_dist_vec(state, ctx, sv, ps, Dist::Dirichlet, AnfDirichlet)
        }
    }
}

pub fn eval_sanfs(
    state: &mut super::eval::State,
    ctx: &Ctx,
    anfs: &[AnfAnn<SVal>],
) -> Result<(Output, Vec<AnfTr<SVal>>)> {
    let (svals, trs) = anfs.iter().fold(Ok((vec![], vec![])), |res, a| {
        let (svals_fin, mut trs_fin) = res?;
        let (o, atr) = eval_sanf(state, ctx, a)?;
        let svals = svals_fin.iter().chain(&o.sample.out).cloned().collect_vec();
        trs_fin.push(atr);
        Ok((svals, trs_fin))
    })?;
    let out = ctx.sample.as_output(svals);
    let out = ctx.mk_soutput(out);

    Ok((out, trs))
}

pub fn eval_eanf<'a>(
    state: &mut super::eval::State,
    ctx: &'a Ctx,
    a: &'a AnfAnn<EVal>,
) -> Result<(EOutput, AnfTr<EVal>)> {
    use Anf::*;
    match a {
        AVal(_, v) => {
            let span = tracing::span!(Level::DEBUG, "val");
            let _enter = span.enter();
            tracing::debug!("val");

            let out = ctx.exact.as_output(vec![v.clone()]);
            Ok((out.clone(), AVal(Output::exact(out), v.clone())))
        }
        AVar(d, s) => {
            let span = tracing::span!(Level::DEBUG, "var");
            let _enter = span.enter();
            tracing::debug!("var");

            let out = eval_eanf_var(state, ctx, d, s)?;
            Ok((out.clone(), AVar(out.pkg(), s.to_string())))
        }
        Neg(bl) => {
            let span = tracing::span!(Level::DEBUG, "neg");
            let _enter = span.enter();
            tracing::debug!("neg");

            let (ol, bl) = eval_eanf(state, ctx, &bl)?;
            match &ol.out[..] {
                [EVal::EBdd(bdd)] => {
                    let out = ctx.exact.as_output(vec![EVal::EBdd(bdd.neg())]);
                    Ok((out.clone(), Neg(Box::new(bl))))
                }
                _ => return errors::typecheck_failed("anf negation"),
            }
        }
        Or(bl, br) => eval_eanf_bop(state, ctx, bl, br, |mgr, l, r| mgr.or(*l, *r), Or),
        And(bl, br) => eval_eanf_bop(state, ctx, bl, br, |mgr, l, r| mgr.and(*l, *r), Or),

        Plus(bl, br) => {
            let span = tracing::span!(Level::DEBUG, "+");
            let _enter = span.enter();
            eval_eanf_numop(state, ctx, bl, br, |a, b| a + b, |a, b| a + b, Plus)
        }
        Minus(bl, br) => {
            let span = tracing::span!(Level::DEBUG, "-");
            let _enter = span.enter();

            eval_eanf_numop(state, ctx, bl, br, |a, b| a - b, |a, b| a - b, Minus)
        }
        Mult(bl, br) => {
            let span = tracing::span!(Level::DEBUG, "*");
            let _enter = span.enter();

            eval_eanf_numop(state, ctx, bl, br, |a, b| a * b, |a, b| a * b, Mult)
        }
        Div(bl, br) => {
            let span = tracing::span!(Level::DEBUG, "/");
            let _enter = span.enter();

            eval_eanf_numop(state, ctx, bl, br, |a, b| a / b, |a, b| a / b, Div)
        }

        GT(bl, br) => eval_eanf_cop(
            state,
            ctx,
            bl,
            br,
            None::<&dyn Fn(&BddPtr, &BddPtr) -> bool>,
            PartialOrd::gt,
            PartialOrd::gt,
            GT,
        ),
        GTE(bl, br) => eval_eanf_cop(
            state,
            ctx,
            bl,
            br,
            None::<&dyn Fn(&BddPtr, &BddPtr) -> bool>,
            PartialOrd::ge,
            PartialOrd::ge,
            GTE,
        ),
        LT(bl, br) => eval_eanf_cop(
            state,
            ctx,
            bl,
            br,
            None::<&dyn Fn(&BddPtr, &BddPtr) -> bool>,
            PartialOrd::lt,
            PartialOrd::lt,
            LT,
        ),
        LTE(bl, br) => eval_eanf_cop(
            state,
            ctx,
            bl,
            br,
            None::<&dyn Fn(&BddPtr, &BddPtr) -> bool>,
            PartialOrd::le,
            PartialOrd::le,
            LTE,
        ),
        EQ(bl, br) => eval_eanf_cop(
            state,
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
                        let (aout, tr) = eval_eanf(state, ctx, a)?;
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
            Ok((ctx.exact.as_output(outs), AnfProd(trs)))
        }
        AnfPrj(var, ix) => {
            let (ovar, var) = eval_eanf(state, ctx, var)?;
            let (oix, ix) = eval_eanf(state, ctx, ix)?;

            match (&ovar.out[..], &oix.out[..]) {
                ([EVal::EProd(vs)], [EVal::EInteger(i)]) => {
                    let out = ctx.exact.as_output(vec![vs[*i].clone()]);
                    Ok((out.clone(), AnfPrj(Box::new(var), Box::new(ix))))
                }
                (vs, [EVal::EInteger(i)]) => {
                    let out = ctx.exact.as_output(vec![vs[*i].clone()]);
                    Ok((out.clone(), AnfPrj(Box::new(var), Box::new(ix))))
                }

                (l, r) => {
                    return errors::typecheck_failed(&format!(
                        "anf projection:\nprod: {:?}\nindex: {:?}",
                        l, r
                    ))
                }
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

pub fn eval_eanfs(
    state: &mut super::eval::State,
    ctx: &Ctx,
    anfs: &[AnfAnn<EVal>],
) -> Result<(EOutput, Vec<AnfTr<EVal>>)> {
    let (vals, trs) = anfs.iter().fold(Ok((vec![], vec![])), |res, a| {
        let (vals_fin, mut trs_fin) = res?;
        let (o, atr) = eval_eanf(state, ctx, a)?;
        let vals = vals_fin.iter().chain(&o.out).cloned().collect_vec();
        trs_fin.push(atr);
        Ok((vals, trs_fin))
    })?;
    Ok((ctx.exact.as_output(vals), trs))
}
