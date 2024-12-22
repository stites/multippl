pub mod render;

use crate::data::HashSet;
use itertools::izip;
use rsdd::builder::bdd_builder::*;
use rsdd::builder::bdd_plan::BddPlan;
use rsdd::builder::cache::all_app::*;
use rsdd::repr::bdd::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::*;

#[inline]
pub fn l1_distance(x0: &[f64], x1: &[f64]) -> f64 {
    izip!(x0, x1).fold(0.0, |dist, (l, r)| dist + (l - r).abs())
}

#[inline]
pub fn get_vec<T: Copy>(v: Vec<T>, i: usize) -> Option<T> {
    if i < v.len() {
        Some(v[i])
    } else {
        None
    }
}
#[inline]
pub fn get_or_else<T: Copy>(v: Vec<T>, i: usize, d: T) -> T {
    get_vec(v, i).unwrap_or(d)
}

pub fn enable_traced_test() -> std::result::Result<(), ::tracing::subscriber::SetGlobalDefaultError>
{
    ::tracing::subscriber::set_global_default(
        ::tracing_subscriber::FmtSubscriber::builder()
            .with_max_level(tracing::Level::DEBUG)
            .without_time()
            .finish(),
    )
}
pub fn leaf_variable(bdd: BddPtr) -> Option<VarLabel> {
    let n = bdd.into_node_safe()?;
    if (n.low == BddPtr::PtrTrue && n.high == BddPtr::PtrFalse)
        || (n.low == BddPtr::PtrFalse && n.high == BddPtr::PtrTrue)
    {
        Some(n.var)
    } else {
        None
    }
}

pub fn variables(bdd: BddPtr) -> Vec<VarLabel> {
    Fold::new(
        &mut |vs: Vec<Option<VarLabel>>, bdd| {
            let mut vs = vs;
            vs.push(bdd.node.var_safe());
            vs
        },
        vec![],
        &|ret, lo_hi| match lo_hi {
            None => ret,
            Some((lo, hi)) => {
                let mut v = ret;
                v.extend(lo);
                v.extend(hi);
                v
            }
        },
    )
    .mut_fold(&bdd)
    .into_iter()
    .flatten()
    .collect()
}

pub fn plan_variables_h(bdd: &BddPlan, mut vs: HashSet<VarLabel>) -> HashSet<VarLabel> {
    match bdd {
        BddPlan::And(a0, a1) => {
            let a0s = plan_variables_h(a0, HashSet::default());
            let a1s = plan_variables_h(a1, HashSet::default());
            vs.extend(a0s);
            vs.extend(a1s);
            vs
        }
        BddPlan::Or(a0, a1) => {
            let a0s = plan_variables_h(a0, HashSet::default());
            let a1s = plan_variables_h(a1, HashSet::default());
            vs.extend(a0s);
            vs.extend(a1s);
            vs
        }
        BddPlan::Iff(a0, a1) => {
            let a0s = plan_variables_h(a0, HashSet::default());
            let a1s = plan_variables_h(a1, HashSet::default());
            vs.extend(a0s);
            vs.extend(a1s);
            vs
        }
        BddPlan::Ite(a0, a1, a2) => {
            let a0s = plan_variables_h(a0, HashSet::default());
            let a1s = plan_variables_h(a1, HashSet::default());
            let a2s = plan_variables_h(a2, HashSet::default());
            vs.extend(a0s);
            vs.extend(a1s);
            vs.extend(a2s);
            vs
        }
        BddPlan::Not(a) => plan_variables_h(a, vs),
        BddPlan::ConstTrue => vs,
        BddPlan::ConstFalse => vs,
        BddPlan::Literal(a, b) => {
            vs.insert(*a);
            vs
        }
    }
}

pub fn plan_variables(bdd: &BddPlan) -> Vec<VarLabel> {
    plan_variables_h(bdd, HashSet::default())
        .into_iter()
        .collect()
}
