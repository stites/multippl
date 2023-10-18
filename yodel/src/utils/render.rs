use crate::annotate::grammar::Var;
use crate::data::*;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::SVal;
use itertools::*;
/// helper functions for rendering
use rsdd::builder::bdd_plan::*;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;
use tracing::*;

pub fn rendervec(fs: &[String]) -> String {
    format!("[{}]", fs.join(", "))
}

pub fn rendersval(s: &SVal) -> String {
    match s {
        SVal::SBool(x) => format!("{}", x),
        SVal::SFloat(x) => format!("{}", x),
        SVal::SInt(x) => format!("{}", x),
        SVal::SVec(xs) => rendersvals(&xs),
        SVal::SProd(xs) => rendersvals(&xs),
        SVal::SDist(x) => format!("{:?}", x),
    }
}

pub fn rendersvals_h(s: &[SVal]) -> Vec<String> {
    s.iter().map(|x| rendersval(x)).collect_vec()
}
pub fn rendersvals(xs: &[SVal]) -> String {
    rendervec(&rendersvals_h(xs))
}
pub fn renderfloats(fs: &[f64], high_prec: bool) -> String {
    rendervec(&fs.iter().map(|x| fmt_f64(high_prec)(*x)).collect_vec())
}

pub fn renderbdds<V: std::fmt::Debug>(fs: &[V]) -> String {
    rendervec(&fs.iter().map(|b| format!("{:?}", b)).collect_vec())
}

pub fn rendervar(var: &Var) -> String {
    format!(
        "({:?})",
        var,
        // var.label
        //     .map_or_else(|| "L-".to_string(), |l| format!("L{}", l.value())),
    )
}
pub fn rendersubs(fs: &HashMap<UniqueId, (Vec<EVal>, Var)>) -> String {
    format!(
        "[{}]",
        &fs.iter()
            .map(|(k, (b, var))| format!("{:?}@{}: {}", k, rendervar(var), renderbdds(b)))
            .collect_vec()
            .join(" ,")
    )
}
pub fn fmt_f64(high_precision: bool) -> impl Fn(f64) -> String {
    if high_precision {
        move |x: f64| format!("{}", x)
    } else {
        move |x: f64| format!("{:.2}", x)
    }
}

pub fn render_weights(ws: &[f64], high_precision: bool) -> String {
    ws.iter()
        .cloned()
        .map(|w| format!("[{}]", fmt_f64(high_precision)(w)))
        .join(", ")
}
pub fn render_probs(ws: &[Probability], high_precision: bool) -> String {
    ws.iter()
        .cloned()
        .map(|w| format!("[{}]", fmt_f64(high_precision)(w.as_f64())))
        .join(", ")
}

pub fn render_history(xss: &[Vec<f64>], high_precision: bool) -> String {
    xss.iter()
        .map(|ws| ws.iter().cloned().map(fmt_f64(high_precision)).join(", "))
        .map(|ws| format!("[{}]", ws))
        .join(", ")
}
pub fn renderw(ws: &WeightMap) -> String {
    ws.weights
        .iter()
        .map(|(k, w)| format!("L{}: ({:.4}, {:.4})", k.value(), w.lo, w.hi))
        .join(", ")
}
pub fn renderp<V: std::fmt::Debug + Clone + PartialEq>(ps: &SubstMap<V>) -> String {
    ps.iter()
        .map(|(k, v)| format!("{k}: {}", renderbdds(&v.val())))
        .join(", ")
}
pub fn renderssubs(ps: &SubstMap<SVal>) -> String {
    ps.iter().map(|(k, x)| format!("{k}: {:?}", x)).join(", ")
}

#[macro_export]
macro_rules! debug_step {
    ($s:expr, $ctx:expr, $comp:expr) => {{
        debug!(
            "{}, [{}], [{}]",
            $s,
            renderp(&$ctx.substitutions),
            renderw(&$ctx.weightmap)
        );
        debug_compiled!($comp);
    }};
}
#[macro_export]
macro_rules! debug_step_ng {
    ($s:expr, $ctx:expr, $comp:expr) => {{
        debug!(
            "{}, e[{}], e[{}], s[{}]",
            $s,
            renderp(&$ctx.exact.substitutions),
            renderw(&$ctx.exact.weightmap),
            renderssubs(&$ctx.sample.substitutions),
        );
        debug_compiled!($comp);
    }};
    ($s:expr, $ctx:expr, $comp:expr ; $type:literal) => {{
        debug!(
            "{}, e[{}], e[{}], s[{}]",
            $s,
            renderp(&$ctx.exact.substitutions),
            renderw(&$ctx.exact.weightmap),
            renderssubs(&$ctx.sample.substitutions),
        );
        if $type == "sample" {
            debug_compiled!(@ $comp.sample);
        } else if $type == "exact" {
            debug_compiled!($comp.exact);
        } else {
            panic!("debug_step_ng with @ \"exact\" or \"sample\", got: {}", $type);
        }
    }};
}
#[macro_export]
macro_rules! debug_compiled {
    ($comp:expr) => {{
        let dists = renderbdds(&$comp.out);
        let accepts = format!("{:?}", $comp.accept);
        // let weights = $comp
        //     .importance
        //     .clone()
        //     .into_iter()
        //     .map(fmt_f64(false))
        //     .join(", ");

        debug!("      \\||/  {}", dists);
        debug!("      \\||/  {}", accepts);
        debug!("      \\||/  [{}]", renderw(&$comp.weightmap));
        debug!("      \\||/  [{}]", renderp(&$comp.substitutions));
        // debug!("      \\||/  {}", weights);
        debug!("----------------------------------------");
    }};
    (@ $comp:expr) => {{
        debug!("      \\||/  {:?}", &$comp.out);
        debug!("      \\||/  {:?}", &$comp.trace);
        debug!("      \\||/  {:?}", &$comp.substitutions);
        debug!("----------------------------------------");
    }};
}
