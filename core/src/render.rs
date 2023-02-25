use crate::annotate::grammar::Var;
use crate::compile::{Compiled, Context, SubstMap, WeightMap};
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use itertools::*;
/// helper functions for rendering
use rsdd::repr::bdd::*;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;
use tracing::*;

pub fn rendervec(fs: &Vec<String>) -> String {
    format!("[{}]", fs.join(", "))
}

pub fn renderfloats(fs: &Vec<f64>, high_prec: bool) -> String {
    rendervec(&fs.iter().map(|x| fmt_f64(high_prec)(*x)).collect_vec())
}

pub fn renderbdds(fs: &Vec<BddPtr>) -> String {
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

pub fn render_weights(ws: &Vec<f64>, high_precision: bool) -> String {
    ws.iter()
        .cloned()
        .map(|w| format!("[{}]", fmt_f64(high_precision)(w)))
        .join(", ")
}
pub fn render_probs(ws: &Vec<Probability>, high_precision: bool) -> String {
    ws.iter()
        .cloned()
        .map(|w| format!("[{}]", fmt_f64(high_precision)(w.as_f64())))
        .join(", ")
}

pub fn render_history(xss: &Vec<Vec<f64>>, high_precision: bool) -> String {
    xss.iter()
        .map(|ws| ws.iter().cloned().map(fmt_f64(high_precision)).join(", "))
        .map(|ws| format!("[{}]", ws))
        .join(", ")
}

#[macro_export]
macro_rules! debug_compiled {
    ($s:expr, $ctx:expr, $comp:expr) => {{
        let renderw = |ws: &WeightMap| {
            ws.weights
                .iter()
                .map(|(k, w)| format!("L{}: ({:.4}, {:.4})", k.value(), w.lo, w.hi))
                .join(", ")
        };
        let renderp = |ps: &SubstMap| {
            ps.iter()
                .map(|(k, (v, _))| format!("{k}: {}", renderbdds(v)))
                .join(", ")
        };

        let p = &$ctx.substitutions;
        let dists = renderbdds(&$comp.dists);
        let accepts = format!("{}", $comp.accept.print_bdd());
        let weights = $comp
            .importance
            .clone()
            .into_iter()
            .map(fmt_f64(false))
            .join(", ");

        debug!("{}, [{}], [{}]", $s, renderp(p), renderw(&$ctx.weightmap));
        debug!("      \\||/  {}", dists);
        debug!("      \\||/  {}", accepts);
        debug!("      \\||/  [{}]", renderw(&$comp.weightmap));
        debug!("      \\||/  [{}]", renderp(&$comp.substitutions));
        debug!("      \\||/  {}", render_probs(&$comp.probabilities, false));
        debug!("      \\||/  {}", weights);
        debug!("----------------------------------------");
    }};
}
