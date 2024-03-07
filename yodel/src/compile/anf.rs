use super::grammar::*;
use crate::annotate::grammar::*;
use crate::data::*;
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
    let sout = ctx.sample.as_output(None);
    let mut out = ctx.mk_soutput(sout);

    match out.sample.substitutions.get(&nv.id()) {
        Some(vs) => {
            out.sample.out = Some(vs.clone());
            Ok(out)
        }
        None => {
            match out.clone().exact.substitutions.get(&nv.id()) {
                None => Err(Generic(format!(
                    "variable {} does not reference known substitution in: s{:?}, e{:?}",
                    nv.name(),
                    out.sample.substitutions,
                    out.exact.substitutions
                ))),
                Some(vs) => match vs {
                    EVal::EBdd(dist) => {
                        let sample = super::sample::exact2sample_bdd_eff(state, &mut out, dist);
                        out.sample.out = Some(SVal::SBool(sample));
                        out.sample.substitutions.insert(
                            nv.id(),
                            SVal::SBool(sample), // ], Some(Var::Named(nv.clone()))),
                        );
                        Ok(out)
                    }
                    _ => {
                        let embedding = SExpr::<Trace>::embed(vs)?;
                        out.sample.out = Some(embedding.clone());
                        out.sample
                            .substitutions
                            // .insert(nv.id(), Subst::mk(embedding, Some(Var::Named(nv.clone()))));
                            .insert(nv.id(), embedding);
                        Ok(out)
                    }
                },
            }
        }
    }
}

pub fn eval_eanf_var(
    state: &mut super::eval::State,
    ctx: &Ctx,
    nv: &NamedVar,
    s: &str,
) -> Result<EOutput> {
    let mut eout = ctx.exact.as_output(None);

    match ctx.exact.substitutions.get(&nv.id()) {
        Some(vs) => {
            eout.out = Some(vs.clone());
            Ok(eout)
        }
        None => match ctx.sample.substitutions.get(&nv.id()) {
            None => Err(Generic(format!(
                "variable {} does not reference known substitution in: s{:?}, e{:?}",
                nv.name(),
                ctx.sample.substitutions,
                ctx.exact.substitutions
            ))),
            Some(vs) => {
                let embedding = EExpr::<Trace>::embed(vs)?;
                // let embedding = vs
                //     .iter()
                //     .map(EExpr::<Trace>::embed)
                //     .collect::<Result<Vec<_>>>()?;
                eout.out = Some(embedding.clone());
                eout.substitutions
                    // .insert(nv.id(), Subst::mk(embedding, Some(Var::Named(nv.clone()))));
                    .insert(nv.id(), embedding);
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
) -> Result<Output> {
    let ol = eval_sanf(state, ctx, bl)?;
    let or = eval_sanf(state, ctx, br)?;
    match (&ol.sample.out, &or.sample.out) {
        (Some(SVal::SFloat(l)), Some(SVal::SFloat(r))) => {
            let out = ctx.sample.as_output(Some(SVal::SFloat(fop(*l, *r))));
            let out = ctx.mk_soutput(out);
            Ok(out)
        }
        (Some(SVal::SInt(l)), Some(SVal::SInt(r))) => {
            let out = ctx.sample.as_output(Some(SVal::SInt(iop(*l, *r))));
            let out = ctx.mk_soutput(out);
            Ok(out)
        }
        _ => errors::typecheck_failed("sanf numop"),
    }
}
pub fn as_bdd(b: &EVal) -> BddPtr {
    match b {
        EVal::EBdd(b) => *b,
        _ => todo!(),
    }
}
#[allow(unreachable_code)] // there are still some todos with bdd integer ops
pub fn eval_eanf_numop<'a>(
    state: &mut super::eval::State,
    ctx: &'a Ctx,
    bl: &'a AnfAnn<EVal>,
    br: &'a AnfAnn<EVal>,
    fop: impl Fn(f64, f64) -> f64,
    iop: impl Fn(usize, usize) -> usize,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<EOutput> {
    let span = tracing::span!(Level::DEBUG, "numop");
    let _enter = span.enter();
    let ol = eval_eanf(state, ctx, bl)?;
    let or = eval_eanf(state, ctx, br)?;
    match (&ol.out, &or.out) {
        (Some(EVal::EFloat(l)), Some(EVal::EFloat(r))) => {
            let x = fop(*l, *r);
            tracing::debug!("{l} <fop> {r} = {x}");
            let out = ctx.exact.as_output(Some(EVal::EFloat(x)));
            Ok(out)
        }
        (Some(EVal::EInteger(l)), Some(EVal::EInteger(r))) => {
            // let l = integers::from_prod(l)?;
            // let r = integers::from_prod(r)?;
            let x = iop(*l, *r);
            tracing::debug!("{l} <iop> {r} = {x}");
            // let out = ctx.exact.as_output(Some(integers::as_onehot(x)));
            let out = ctx.exact.as_output(Some(EVal::EInteger(x)));
            Ok(out)
        }
        (Some(EVal::EProd(bdds)), Some(EVal::EInteger(r))) => {
            todo!("this one is actually just a left/right shift with false to the OH vector");
            // let l = bdds.iter().position(|b| !as_bdd(&b).is_neg()).unwrap();
            // let x = iop(l, *r);
            // assert!(x < bdds.len());
            // tracing::debug!("{l} <iop> {r} = {x}");
            // // let out = ctx.exact.as_output(Some(integers::as_onehot(x)));
            // let plus = bdds
            //     .iter()
            //     .enumerate()
            //     .map(|(ix, b)| {
            //         let b = as_bdd(&b);
            //         if ix == x && b.is_neg() {
            //            b.neg() // make positive
            //         } else if !b.is_neg() {
            //             b.neg() // make everything else positive
            //         } else {
            //             b
            //         }
            //     })
            //     .map(EVal::EBdd)
            //     .collect_vec();
            // let out = ctx.exact.as_output(Some(EVal::EProd(plus)));
            // Ok(out)
        }
        // (Some(EVal::EInteger(l)], _) => return errors::erased(),
        // (_, [EVal::EInteger(r))) => return errors::erased(),
        (l, r) => errors::typecheck_failed(&format!(
            "eanf numop. Arguments:\nleft: {:?}\nright: {:?}",
            l, r
        )),
    }
}
pub fn eval_sanf_bop<'a>(
    state: &mut super::eval::State,
    ctx: &'a Ctx,
    bl: &'a AnfAnn<SVal>,
    br: &'a AnfAnn<SVal>,
    bop: impl Fn(bool, bool) -> bool,
    op: impl Fn(Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<Output> {
    let ol = eval_sanf(state, ctx, bl)?;
    let or = eval_sanf(state, ctx, br)?;
    match (&ol.sample.out, &or.sample.out) {
        (Some(SVal::SBool(l)), Some(SVal::SBool(r))) => {
            let out = ctx.sample.as_output(Some(SVal::SBool(bop(*l, *r))));
            let out = ctx.mk_soutput(out);
            Ok(out)
        }
        (l, r) => errors::typecheck_failed(&format!(
            "sanf boolop. Arguments:\nleft: {:?}\nright: {:?}",
            l, r
        )),
    }
}
pub fn eval_eanf_bop(
    state: &mut super::eval::State,
    ctx: &Ctx,
    bl: &AnfAnn<EVal>,
    br: &AnfAnn<EVal>,
    mut bop: impl FnMut(&mut Mgr, Box<BddPtr>, Box<BddPtr>) -> BddPtr,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<EOutput> {
    let span = tracing::span!(Level::DEBUG, "binop");
    let _enter = span.enter();
    tracing::debug!("binop");

    let ol = eval_eanf(state, ctx, bl)?;
    let or = eval_eanf(state, ctx, br)?;
    match (&ol.out, &or.out) {
        (Some(EVal::EBdd(l)), Some(EVal::EBdd(r))) => {
            let out =
                ctx.exact
                    .as_output(Some(EVal::EBdd(bop(state.mgr, Box::new(*l), Box::new(*r)))));
            Ok(out)
        }
        _ => errors::typecheck_failed("eanf boolop"),
    }
}

#[allow(clippy::too_many_arguments)]
pub fn eval_sanf_cop(
    state: &mut super::eval::State,
    ctx: &Ctx,
    bl: &AnfAnn<SVal>,
    br: &AnfAnn<SVal>,
    bop: impl Fn(&bool, &bool) -> bool,
    fop: impl Fn(&f64, &f64) -> bool,
    iop: impl Fn(&u64, &u64) -> bool,
    op: impl Fn(Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<Output> {
    let ol = eval_sanf(state, ctx, bl)?;
    let or = eval_sanf(state, ctx, br)?;
    match (&ol.sample.out, &or.sample.out) {
        (Some(SVal::SBool(l)), Some(SVal::SBool(r))) => {
            let out = ctx.sample.as_output(Some(SVal::SBool(bop(l, r))));
            let out = ctx.mk_soutput(out);
            Ok(out)
        }
        (Some(SVal::SFloat(l)), Some(SVal::SFloat(r))) => {
            let out = ctx.sample.as_output(Some(SVal::SBool(fop(l, r))));
            let out = ctx.mk_soutput(out);
            Ok(out)
        }
        (Some(SVal::SInt(l)), Some(SVal::SInt(r))) => {
            let out = ctx.sample.as_output(Some(SVal::SBool(iop(l, r))));
            let out = ctx.mk_soutput(out);
            Ok(out)
        }
        (Some(SVal::SInt(l)), Some(SVal::SFloat(r))) => {
            let out = ctx
                .sample
                .as_output(Some(SVal::SBool(fop(&(*l as f64), r))));
            let out = ctx.mk_soutput(out);
            Ok(out)
        }
        (Some(SVal::SFloat(l)), Some(SVal::SInt(r))) => {
            let out = ctx
                .sample
                .as_output(Some(SVal::SBool(fop(l, &(*r as f64)))));
            let out = ctx.mk_soutput(out);
            Ok(out)
        }
        (l, r) => {
            errors::typecheck_failed(&format!("sanf compare op:\nleft: {:?}\nright: {:?}", l, r))
        }
    }
}

#[allow(clippy::too_many_arguments)]
#[allow(unreachable_code)] // there are still some todos
pub fn eval_eanf_cop(
    state: &mut super::eval::State,
    ctx: &Ctx,
    bl: &AnfAnn<EVal>,
    br: &AnfAnn<EVal>,
    bop: Option<impl FnMut(&mut Mgr, Box<BddPtr>, Box<BddPtr>) -> BddPtr>,
    fop: impl Fn(&f64, &f64) -> bool,
    iop: impl Fn(&usize, &usize) -> bool,
    op: impl Fn(Box<AnfTr<EVal>>, Box<AnfTr<EVal>>) -> AnfTr<EVal>,
) -> Result<EOutput> {
    let ol = eval_eanf(state, ctx, bl)?;
    let or = eval_eanf(state, ctx, br)?;
    match (&ol.out, &or.out) {
        (Some(EVal::EBdd(l)), Some(EVal::EBdd(r))) => match bop {
            None => errors::typecheck_failed("eanf compare op invalid"),
            Some(mut bop) => {
                let out = ctx.exact.as_output(Some(EVal::EBdd(bop(
                    state.mgr,
                    Box::new(*l),
                    Box::new(*r),
                ))));
                Ok(out)
            }
        },
        (Some(EVal::EFloat(l)), Some(EVal::EFloat(r))) => {
            let out = ctx
                .exact
                .as_output(Some(EVal::EBdd(BddPtr::from_bool(fop(l, r)))));
            Ok(out)
        }
        (Some(EVal::EInteger(l)), Some(EVal::EInteger(r))) => {
            let out = ctx
                .exact
                .as_output(Some(EVal::EBdd(BddPtr::from_bool(iop(l, r)))));
            Ok(out)
        }
        (Some(EVal::EProd(bdds)), Some(EVal::EInteger(r))) => {
            match bop {
                None => {
                    todo!("This one is just a disjunction of (bdd <=> T/F) with the OH encoded integer.");
                    // this is type-safe, so we can perform this operation
                    let l = bdds.iter().position(|b| !as_bdd(b).is_neg()).unwrap();
                    let out = ctx
                        .exact
                        .as_output(Some(EVal::EBdd(BddPtr::from_bool(iop(&l, r)))));
                    Ok(out)
                }
                Some(mut bop) => {
                    // println!("comparing {:?} == {}", bdds, r);
                    let oh = crate::desugar::integers::as_onehot_(*r);
                    let fin = izip!(bdds, oh).fold(Ok(BddPtr::PtrTrue), |acc, (b, o)| match b {
                        EVal::EBdd(b) => {
                            let apply = bop(state.mgr, Box::new(*b), Box::new(o));
                            Ok(state.mgr.and(acc?, apply))
                        }
                        _ => errors::typecheck_failed("eanf compare op invalid"),
                    })?;
                    let out = ctx.exact.as_output(Some(EVal::EBdd(fin)));
                    Ok(out)
                }
            }
        }
        (Some(EVal::EInteger(r)), Some(EVal::EProd(bdds))) => {
            todo!();
        }
        (l, r) => errors::typecheck_failed(&format!(
            "eanf compare op between:\n left: {:?}\nright: {:?}\n",
            l, r
        )),
    }
}
pub fn eval_sanf_vec(
    state: &mut super::eval::State,
    ctx: &Ctx,
    anfs: &[AnfAnn<SVal>],
    op: impl Fn(Vec<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<Output> {
    let outs: Vec<SVal> = anfs
        .iter()
        .map(|a| {
            let aout = eval_sanf(state, ctx, a)?;
            let ov = aout.sample.out.unwrap();
            Ok(ov)
        })
        .collect::<Result<Vec<SVal>>>()?;
    tracing::debug!("sanf_vec: {outs:?}");
    let o = ctx.sample.as_output(Some(SVal::SVec(outs)));
    let o = ctx.mk_soutput(o);
    Ok(o)
}
fn eval_sanf_dist2(
    state: &mut super::eval::State,
    ctx: &Ctx,
    sv: &SampledVar,
    p0: &AnfAnn<SVal>,
    p1: &AnfAnn<SVal>,
    mkdist: impl Fn(f64, f64) -> Dist,
    mkanf: impl Fn((SampledVar, SOutput), Box<AnfTr<SVal>>, Box<AnfTr<SVal>>) -> AnfTr<SVal>,
) -> Result<Output> {
    let o0 = eval_sanf(state, ctx, p0)?;
    let o1 = eval_sanf(state, ctx, p1)?;
    match (&o0.sample.out, &o1.sample.out) {
        (Some(SVal::SFloat(l)), Some(SVal::SFloat(r))) => {
            let out = ctx.sample.as_output(Some(SVal::SDist(mkdist(*l, *r))));

            let out = ctx.mk_soutput(out);
            let ext = (sv.clone(), out.sample.clone());
            Ok(out)
        }
        (l, r) => {
            tracing::debug!(" left: {l:?}");
            tracing::debug!("right: {r:?}");

            errors::typecheck_failed("sanf dist with 2 args not given correct arguments.")
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
) -> Result<Output> {
    let o = eval_sanfs(state, ctx, ps)?;
    match &o.sample.out {
        Some(SVal::SVec(vs)) => {
            let vs = vals2vec_params(vs)?;
            let out = ctx.sample.as_output(Some(SVal::SDist(mkdist(vs))));
            let out = ctx.mk_soutput(out);
            let ext = (sv.clone(), out.sample.clone());
            Ok(out)
        }
        _ => errors::typecheck_failed("sanf distribution was not vector args not given"),
    }
}
pub fn eval_sanf<'a>(
    state: &'a mut super::eval::State,
    ctx: &'a Ctx,
    a: &'a AnfAnn<SVal>,
) -> Result<Output> {
    use Anf::*;
    let span = tracing::span!(tracing::Level::DEBUG, "eval_sanf");
    let _enter = span.enter();
    match a {
        AVal(_, v) => {
            tracing::debug!("aval  in: {v:?}");
            let out = ctx.sample.as_output(Some(v.clone()));
            let out = ctx.mk_soutput(out);
            tracing::debug!("aval out: {out:?}");
            Ok(out)
        }
        AVar(d, s) => {
            tracing::debug!("avar: {d:?} -- {s:?}");
            let out = eval_sanf_var(state, ctx, d, s)?;
            Ok(out)
        }
        Neg(bl) => {
            let ol = eval_sanf(state, ctx, bl)?;
            match &ol.sample.out {
                Some(SVal::SBool(l)) => {
                    let out = ctx.sample.as_output(Some(SVal::SBool(!*l)));
                    let out = ctx.mk_soutput(out);
                    Ok(out)
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
        AnfPush(xs, x) => {
            let xs = eval_sanf(state, ctx, xs)?;
            let x = eval_sanf(state, ctx, x)?;
            let push = |xs: &Vec<SVal>, x: &SVal| {
                let mut rs = xs.clone();
                rs.push(x.clone());
                rs
            };
            match (&xs.sample.out, &x.sample.out) {
                (Some(SVal::SVec(xs)), Some(x)) => {
                    let out = ctx.sample.as_output(Some(SVal::SVec(push(xs, x))));
                    let out = ctx.mk_soutput(out);
                    Ok(out)
                }
                a => errors::typecheck_failed(&format!("vec push got {a:?}")),
            }
        }
        AnfHead(xs) => {
            let xs = eval_sanf(state, ctx, xs)?;
            match &xs.sample.out {
                Some(SVal::SVec(xs)) => {
                    let out = ctx.sample.as_output(Some(xs[0].clone()));
                    let out = ctx.mk_soutput(out);
                    Ok(out)
                }
                a => errors::typecheck_failed(&format!("vec head got {a:?}")),
            }
        }
        AnfTail(xs) => {
            let xs = eval_sanf(state, ctx, xs)?;
            match &xs.sample.out {
                Some(SVal::SVec(xs)) => {
                    let (_, tail) = xs.split_first().unwrap();
                    let out = ctx.sample.as_output(Some(SVal::SVec(tail.to_vec())));
                    let out = ctx.mk_soutput(out);
                    Ok(out)
                }
                a => errors::typecheck_failed(&format!("vec head got {a:?}")),
            }
        }
        AnfTrace(tr, x) => {
            println!(
                "S) {}",
                eval_sanf(state, ctx, tr)?.sample.out.unwrap().pretty()
            );
            eval_sanf(state, ctx, x)
        }
        AnfPrj(var, ix) => {
            let ovar = eval_sanf(state, ctx, var)?;
            let oix = eval_sanf(state, ctx, ix)?;
            match (&ovar.sample.out, &oix.sample.out) {
                (Some(SVal::SProd(vs)), Some(SVal::SInt(i))) => {
                    let out = ctx.sample.as_output(Some(vs[*i as usize].clone()));
                    let out = ctx.mk_soutput(out);
                    Ok(out)
                }
                (Some(SVal::SVec(vs)), Some(SVal::SInt(i))) => {
                    let out = ctx.sample.as_output(Some(vs[*i as usize].clone()));
                    let out = ctx.mk_soutput(out);
                    Ok(out)
                }
                a => errors::typecheck_failed(&format!("anf projection got {a:?}")),
            }
        }
        AnfBernoulli(sv, p) => {
            let o = eval_sanf(state, ctx, p)?;
            match &o.sample.out {
                Some(SVal::SFloat(f)) => {
                    let out = ctx.sample.as_output(Some(SVal::SDist(Dist::Bern(*f))));
                    let out = ctx.mk_soutput(out);
                    let ext = (sv.clone(), out.sample.clone());
                    Ok(out)
                }
                a => errors::typecheck_failed(&format!("anf bernoulli got {a:?}")),
            }
        }
        AnfPoisson(sv, p) => {
            let o = eval_sanf(state, ctx, p)?;
            match &o.sample.out {
                Some(SVal::SInt(i)) => {
                    let out = ctx
                        .sample
                        .as_output(Some(SVal::SDist(Dist::Poisson(*i as f64))));
                    let out = ctx.mk_soutput(out);
                    let ext = (sv.clone(), out.sample.clone());
                    Ok(out)
                }
                Some(SVal::SFloat(f)) => {
                    let out = ctx.sample.as_output(Some(SVal::SDist(Dist::Poisson(*f))));
                    let out = ctx.mk_soutput(out);
                    let ext = (sv.clone(), out.sample.clone());
                    Ok(out)
                }
                a => errors::typecheck_failed(&format!("anf poisson got {a:?}")),
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
) -> Result<Output> {
    let svals = anfs
        .iter()
        .map(|a| {
            let o = eval_sanf(state, ctx, a)?;
            let svals = o.sample.out.unwrap();
            Ok(svals)
        })
        .collect::<Result<Vec<_>>>()?;
    let out = ctx.sample.as_output(Some(SVal::SVec(svals)));
    let out = ctx.mk_soutput(out);
    Ok(out)
}

pub fn eval_eanf<'a>(
    state: &mut super::eval::State,
    ctx: &'a Ctx,
    a: &'a AnfAnn<EVal>,
) -> Result<EOutput> {
    use Anf::*;
    match a {
        AVal(_, v) => {
            let span = tracing::span!(Level::DEBUG, "val");
            let _enter = span.enter();
            tracing::debug!("val");

            let out = ctx.exact.as_output(Some(v.clone()));
            Ok(out)
        }
        AVar(d, s) => {
            let span = tracing::span!(Level::DEBUG, "var");
            let _enter = span.enter();
            tracing::debug!("var");

            let out = eval_eanf_var(state, ctx, d, s)?;
            Ok(out)
        }
        Neg(bl) => {
            let span = tracing::span!(Level::DEBUG, "neg");
            let _enter = span.enter();
            tracing::debug!("neg");

            let ol = eval_eanf(state, ctx, bl)?;
            match &ol.out {
                Some(EVal::EBdd(bdd)) => {
                    let out = ctx.exact.as_output(Some(EVal::EBdd(bdd.neg())));
                    Ok(out)
                }
                _ => errors::typecheck_failed("anf negation"),
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
            None::<&dyn Fn(&mut Mgr, Box<BddPtr>, Box<BddPtr>) -> BddPtr>,
            PartialOrd::gt,
            PartialOrd::gt,
            GT,
        ),
        GTE(bl, br) => eval_eanf_cop(
            state,
            ctx,
            bl,
            br,
            None::<&dyn Fn(&mut Mgr, Box<BddPtr>, Box<BddPtr>) -> BddPtr>,
            PartialOrd::ge,
            PartialOrd::ge,
            GTE,
        ),
        LT(bl, br) => eval_eanf_cop(
            state,
            ctx,
            bl,
            br,
            None::<&dyn Fn(&mut Mgr, Box<BddPtr>, Box<BddPtr>) -> BddPtr>,
            PartialOrd::lt,
            PartialOrd::lt,
            LT,
        ),
        LTE(bl, br) => eval_eanf_cop(
            state,
            ctx,
            bl,
            br,
            None::<&dyn Fn(&mut Mgr, Box<BddPtr>, Box<BddPtr>) -> BddPtr>,
            PartialOrd::le,
            PartialOrd::le,
            LTE,
        ),
        EQ(bl, br) => eval_eanf_cop(
            state,
            ctx,
            bl,
            br,
            Some(|mgr: &mut Mgr, l: Box<BddPtr>, r: Box<BddPtr>| mgr.iff(*l, *r)),
            PartialEq::eq,
            PartialEq::eq,
            EQ,
        ),

        AnfProd(anfs) => {
            let outs: Vec<EVal> = anfs
                .iter()
                .map(|a| {
                    let aout = eval_eanf(state, ctx, a)?;
                    Ok(aout.out.unwrap())
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(ctx.exact.as_output(Some(EVal::EProd(outs))))
        }
        AnfPrj(var, ix) => {
            let ovar = eval_eanf(state, ctx, var)?;
            let oix = eval_eanf(state, ctx, ix)?;

            match (&ovar.out, &oix.out) {
                (Some(EVal::EProd(vs)), Some(EVal::EInteger(i))) => {
                    let out = ctx.exact.as_output(Some(vs[*i].clone()));
                    Ok(out)
                }
                (l, r) => errors::typecheck_failed(&format!(
                    "anf projection:\nprod: {:?}\nindex: {:?}",
                    l, r
                )),
            }
        }
        AnfTrace(tr, x) => {
            println!("E) {}", eval_eanf(state, ctx, tr)?.out.unwrap().pretty());
            eval_eanf(state, ctx, x)
        }
        AnfVec(anfs) => errors::not_in_exact(),
        AnfPush(xs, x) => errors::not_in_exact(),
        AnfHead(xs) => errors::not_in_exact(),
        AnfTail(xs) => errors::not_in_exact(),

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
) -> Result<EOutput> {
    let vals = anfs
        .iter()
        .map(|a| {
            let o = eval_eanf(state, ctx, a)?;
            let vals = o.out.unwrap();
            Ok(vals)
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(ctx.exact.as_output(Some(EVal::EProd(vals))))
}
