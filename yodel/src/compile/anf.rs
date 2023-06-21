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
    match v {
        SVal::SBool(b) => Output::from_anf_dists(ctx, vec![BddPtr::from_bool(*b)]),
        _ => Output::from_anf_dists(ctx, vec![]),
    }
}
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
