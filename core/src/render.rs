use crate::annotate::grammar::Var;
use crate::compile::{Compiled, Context, SubstMap, WeightMap};
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use itertools::*;
/// helper functions for rendering
use rsdd::repr::bdd::*;
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

pub fn render_history(xss: &Vec<Vec<f64>>, high_precision: bool) -> String {
    xss.iter()
        .map(|ws| ws.iter().cloned().map(fmt_f64(high_precision)).join(", "))
        .map(|ws| format!("[{}]", ws))
        .join(", ")
}

pub fn debug_compiled(s: &str, ctx: &Context, c: &Compiled) {
    let p = &ctx.substitutions;
    let renderw = |ws: &WeightMap| {
        ws.iter()
            .map(|(k, (l, h))| format!("{k}: ({l}, {h})"))
            .join(", ")
    };
    let renderp = |ps: &SubstMap| {
        ps.iter()
            .map(|(k, (v, _))| format!("{k}: {}", renderbdds(v)))
            .join(", ")
    };

    let dists = renderbdds(&c.dists);

    let accepts = format!("{}", c.accept.print_bdd());

    debug!("{s}, [{}]", renderp(p));
    debug!("      \\||/  {}", dists);
    debug!("      \\||/  {}", accepts);
    debug!("      \\||/  {}", renderp(&c.substitutions));
    debug!("      \\||/  {}", fmt_f64(false)(c.importance_weight));
    debug!("----------------------------------------");
}
