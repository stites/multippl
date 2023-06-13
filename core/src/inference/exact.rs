pub use crate::inference::wmc::*;
pub use crate::*;

pub fn exact_with(p: &ProgramInferable) -> (Vec<f64>, WmcStats) {
    let p = p.strip_samples();
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
