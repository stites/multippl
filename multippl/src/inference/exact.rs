pub use crate::inference::wmc::*;
pub use crate::*;

pub fn exact_with(p: &str) -> (Vec<f64>, WmcStats) {
    let (p, mstats) = exact_with_h(p);
    (
        p,
        mstats.expect("exact compilation must occur, but did not"),
    )
}

pub fn exact_with_h(p: &str) -> (Vec<f64>, Option<WmcStats>) {
    let o = crate::run!(p; --split exact);
    let (out, mut mgr) = (o.out, o.mgr);
    wmc_prob(&mut mgr, &o.wmcp, &out.exact)
}

pub fn exact(p: &str) -> Vec<f64> {
    exact_with(p).0
}

pub fn exact_inferable(p: &ProgramInferable) -> (Vec<f64>, Option<WmcStats>) {
    let mut opt = crate::pipeline::Options::stoch();
    opt.exact_only = true;
    let exact = p.strip_samples().ok().unwrap();
    let (mut mgr, p, lenv) = crate::pipeline::make_mgr_and_ir_h(&exact).ok().unwrap();
    tracing::debug!(",====================================.");
    tracing::debug!("| manager compiled! building program |");
    tracing::debug!("`===================================='");
    let wmc = WmcP::new_with_size(mgr.get_order().num_vars());
    let r = crate::pipeline::runner(&mut mgr, &mut opt.rng(), wmc, &opt, &p, &lenv)
        .ok()
        .unwrap();
    let o = PartialROut::to_rout(r, mgr);
    let (out, mut mgr) = (o.out, o.mgr);
    let wmcp = o.wmcp;
    wmc_prob(&mut mgr, &wmcp, &out.exact)
}
