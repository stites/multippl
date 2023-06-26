pub use crate::inference::wmc::*;
pub use crate::*;

pub fn exact_with(p: &ProgramInferable) -> (Vec<f64>, WmcStats) {
    let (p, mstats) = exact_with_h(p);
    (
        p,
        mstats.expect("exact compilation must occur, but did not"),
    )
}

pub fn exact_with_h(p: &ProgramInferable) -> (Vec<f64>, Option<WmcStats>) {
    let p = p.strip_samples().unwrap();
    match crate::run(&p) {
        Ok((mut mgr, c)) => wmc_prob(&mut mgr, &c),
        Err(e) => panic!(
            "\nCompiler Error!!!\n==============\n{}\n==============\n",
            e
        ),
    }
}

pub fn exact(p: &ProgramInferable) -> Vec<f64> {
    exact_with(p).0
}
