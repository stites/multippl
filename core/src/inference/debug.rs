use crate::uniquify::grammar::*;
use crate::utils::render::*;
use crate::*;
use itertools::*;
use std::collections::HashMap;
use tracing::debug;

#[allow(clippy::too_many_arguments)]
pub fn debug_importance_weighting(
    high_precision: bool,
    steps: usize,
    ws: &[f64],
    qss: &[Vec<f64>],
    pss: &[Vec<f64>],
    ss: &[HashMap<UniqueId, Vec<bool>>],
    exp: &[f64],
    expw: &[f64],
) {
    if steps < 1001 {
        debug!("");
        debug!("ws = {}", render_weights(ws, false));
        debug!("qs = {}", render_history(qss, false));
        debug!("ps = {}", render_history(pss, false));
        debug!(
            "ss = [{}]",
            ss.iter()
                .map(|kvs| format!(
                    "[{}]",
                    kvs.iter()
                        .map(|(k, vs)| format!("{:?}:{:?}", k, vs))
                        .join(",")
                ))
                .join(", ")
        );
    }
    debug!("ws    = {}", ws.iter().sum::<f64>());
    debug!(exp = renderfloats(exp, false));
    debug!(expw = renderfloats(expw, false));
    izip!(exp, expw).enumerate().for_each(|(i, (exp, expw))| {
        let num = format!("{}", exp);
        let denom = format!("{}", expw);
        let num_len: i32 = num.len().try_into().unwrap();
        let denom_len: i32 = denom.len().try_into().unwrap();

        debug!("computed:");
        debug!("{}", num);
        debug!("{} = {:.4}", "-".repeat(num.len()), (exp / expw));
        let leftpad: usize = ((num_len - denom_len).abs() / 2).try_into().unwrap();
        debug!("{}{}", " ".repeat(leftpad), denom);
        debug!("");
    });
}
