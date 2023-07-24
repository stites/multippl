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
    match crate::run(&p) {
        Ok(o) => {
            let (out, mut mgr, pq) = (o.out, o.mgr, o.pq);
            wmc_prob(&mut mgr, &out.exact)
        }
        Err(e) => panic!(
            "\nCompiler Error!!!\n==============\n{}\n==============\n",
            e
        ),
    }
}

pub fn exact(p: &str) -> Vec<f64> {
    exact_with(p).0
}
