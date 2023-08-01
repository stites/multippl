use crate::compile::*;
use crate::grammar::*;
use crate::inference::*;
use crate::tests::checks::*;
use crate::typeinf::grammar::*;
use crate::*;

use itertools::*;
use rsdd::sample::probability::*;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::ops::Range;
use tracing::*;
use tracing_test::*;

mod checks;
mod freevars;
mod grids;
mod ite;
mod mls;
mod nested;
mod tuples;

#[test]
fn program00() {
    check_exact("p00", vec![1.0], "exact { let x = true in x }");
}
#[test]
fn program01() {
    check_exact1("p01", 1.0 / 3.0, "exact {let x = flip 1.0 / 3.0 in x}");
}
#[test]
fn program02() {
    let mk02 = |ret: &str| {
        r#" exact {
    let x = flip 1.0 / 3.0 in
    let y = flip 1.0 / 4.0 in
    "#
        .to_owned()
            + ret
            + "\n}"
    };

    check_exact1("p02/y  ", 3.0 / 12.0, &mk02("y"));
    check_exact1("p02/x  ", 4.0 / 12.0, &mk02("x"));
    check_exact1("p02/x|y", 6.0 / 12.0, &mk02("x || y"));
    check_exact1("p02/x&y", 1.0 / 12.0, &mk02("x && y"));
}

#[test]
fn program03() {
    let mk = |ret: &str| {
        r#" exact {
    let x = flip 1.0 / 3.0 in
    let y = flip 1.0 / 4.0 in
    let _ = observe x || y in
    "#
        .to_owned()
            + ret
            + "\n}"
    };
    check_exact1("p03/y  ", 3.0 / 6.0, &mk("y"));
    check_exact1("p03/x  ", 4.0 / 6.0, &mk("x"));
    check_exact1("p03/x|y", 6.0 / 6.0, &mk("x || y"));
    check_exact1("p03/x&y", 1.0 / 6.0, &mk("x && y"));
}

#[test]
// #[traced_test]
fn program04_approx() {
    let mk = |ret: &str| {
        r#"exact {
    let x = sample { ~bern(1.0 / 3.0) } in
    let y = flip 1.0 / 4.0 in
    let _ = observe x || y in
    "#
        .to_owned()
            + ret
            + "\n}"
    };

    let n = 2000;
    check_approx1("p04s/y  ", 3.0 / 6.0, &mk("y"), n);
    check_approx1("p04s/x  ", 4.0 / 6.0, &mk("x"), n);
    check_approx1("p04s/x|y", 6.0 / 6.0, &mk("x || y"), n);
    check_approx1("p04s/x&y", 1.0 / 6.0, &mk("x && y"), n);
    // check_invariant("p04", None, None, &mk(&allmarg("y", "x")));
}

// #[test]
// #[ignore]
// #[traced_test]
// fn optimization_eliminates_variables() {
//     let opt = Options {
//         opt: true,
//         debug: false,
//         seed: Some(3),
//     };
//     let mk = |ret: EExprInferable| {
//         Program::EBody(lets![
//             "x" ;= flip!(1/3);
//             "y" ;= sample!(
//                 lets![
//                     "x0" ;= flip!(1/5);
//                     ...? b!("x0" || "x")
//                 ]);
//            "_" ;= observe!(b!("x" || "y")); // is this a problem?
//            ...? ret

//         ])
//     };
//     // (|p| check_invariant("free2/x*y ", None, None, &p))(mk(q!("x" x "y")));
//     // (|p| debug_approx("free2/x*y", vec![0.714285714], &p, 5))(mk(b!("x")));
//     let px = mk(var!("x"));
//     let py = mk(var!("y"));
//     let mut mgr = crate::make_mgr(&py);

//     // let (approx, _) = importance_weighting_h(
//     //     1,
//     //     &py,
//     //     &opt,
//     // );
//     let (cs, inv, sis) = crate::runner_h(&px, &mut mgr, &opt).unwrap();
//     cs.into_iter().for_each(|c| {
//         assert_eq!(c.samples_opt.len(), 1);
//         let (ps, stats) = wmc_prob_opt(&mut mgr, &c, &inv, &sis);
//         debug!("{:?}", ps);
//         debug!("{:#?}", stats);
//     });
//     let mk = Program::EBody(lets![
//         "x" ;= flip!(1/3);
//         "y" ;= sample!(flip!(1/5));
//        ...? var!("y")

//     ]);
//     let mut mgr = crate::make_mgr(&mk);
//     let (cs, inv, sis) = crate::runner_h(&mk, &mut mgr, &opt).unwrap();
//     cs.into_iter().for_each(|c| {
//         assert_eq!(c.samples_opt.len(), 1);
//         let (ps, stats) = wmc_prob_opt(&mut mgr, &c, &inv, &sis);
//         debug!("{:?}", ps);
//         debug!("{:#?}", stats);
//     });

//     // =======================  above is correct ================== //
//     let mk = Program::EBody(lets![
//         "x" ;= flip!(1/3);
//         "tpl" ;= sample!(
//                 lets![
//                     "t0" ;= flip!(1/2);
//                     "t1" ;= flip!(1/2);
//                     ...? b!("t0", "t1")
//                 ]);
//         "fin" ;= fst!("tpl");
//        ...? var!("fin")

//     ]);
//     let mut mgr = crate::make_mgr(&mk);
//     let (cs, inv) = crate::runner_h(&mk, &mut mgr, &opt).unwrap();
//     cs.into_iter().for_each(|c| {
//         assert_eq!(c.samples_opt.len(), 2);
//         let (ps, stats) = wmc_prob_opt(&mut mgr, &c, &inv, &sis);
//         debug!("{:?}", ps);
//         debug!("{:#?}", stats);
//     });
//     // ===================  below is incorrect ================== //
//     use crate::grids::*;
//     let mk_probability = |_ix, _p| Probability::new(0.5);
//     let schema = GridSchema::new_from_fn(
//         2,
//         true,
//         None,
//         None,
//         Default::default(),
//         None,
//         &mk_probability,
//     );
//     let grid = make::grid(schema);
//     let mut mgr = crate::make_mgr(&grid);
//     let (cs, inv, sis) = crate::runner_h(&grid, &mut mgr, &opt).unwrap();
//     cs.into_iter().for_each(|c| {
//         let (ps, stats) = wmc_prob_opt(&mut mgr, &c, &inv, &sis);
//         debug!("{:?}", ps);
//         debug!("{:#?}", stats);
//         todo!();
//     });
// }
