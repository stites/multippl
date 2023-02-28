use crate::annotate::grammar::Var;
use crate::compile::*;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use itertools::*;
/// helper functions for rendering
use rsdd::repr::bdd::*;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;
use tracing::*;

pub fn rendervec(fs: &[String]) -> String {
    format!("[{}]", fs.join(", "))
}

pub fn renderfloats(fs: &[f64], high_prec: bool) -> String {
    rendervec(&fs.iter().map(|x| fmt_f64(high_prec)(*x)).collect_vec())
}

pub fn renderbdds(fs: &[BddPtr]) -> String {
    rendervec(&fs.iter().map(|b| b.print_bdd()).collect_vec())
}

pub fn rendervar(var: &Var) -> String {
    format!(
        "({}{})",
        var.id,
        var.label
            .map_or_else(|| "L-".to_string(), |l| format!("L{}", l.value())),
    )
}
pub fn rendersubs(fs: &HashMap<UniqueId, (Vec<BddPtr>, Var)>) -> String {
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
pub fn renderp(ps: &SubstMap) -> String {
    ps.iter()
        .map(|(k, (v, _))| format!("{k}: {}", renderbdds(v)))
        .join(", ")
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
macro_rules! debug_compiled {
    ($comp:expr) => {{
        let dists = renderbdds(&$comp.dists);
        let accepts = format!("{}", $comp.accept.print_bdd());
        let weights = $comp
            .importance
            .clone()
            .into_iter()
            .map(fmt_f64(false))
            .join(", ");

        debug!("      \\||/  {}", dists);
        debug!("      \\||/  {}", accepts);
        debug!("      \\||/  [{}]", renderw(&$comp.weightmap));
        debug!("      \\||/  [{}]", renderp(&$comp.substitutions));
        debug!("      \\||/  {}", render_probs(&$comp.probabilities, false));
        debug!("      \\||/  {}", weights);
        debug!("----------------------------------------");
    }};
}
