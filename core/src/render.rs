use crate::compile::semantics::UniqueId;
use crate::grammar::*;
use itertools::*;
/// helper functions for rendering
use rsdd::repr::bdd::*;
use std::collections::HashMap;

pub fn rendervec(fs: &Vec<String>) -> String {
    format!("[{}]", fs.join(", "))
}

pub fn renderfloats(fs: &Vec<f64>, high_prec: bool) -> String {
    rendervec(&fs.iter().map(|x| fmt_f64(high_prec)(*x)).collect_vec())
}

pub fn renderbdds(fs: &Vec<BddPtr>) -> String {
    rendervec(&fs.iter().map(|b| b.print_bdd()).collect_vec())
}

pub fn rendersubs(fs: &HashMap<UniqueId, Vec<BddPtr>>) -> String {
    format!(
        "[{}]",
        &fs.iter()
            .map(|(k, b)| format!("{:?}: {}", k, renderbdds(b)))
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
