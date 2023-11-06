use super::*;
use crate::*;

// #[test]
// // #[traced_test]
// fn mls_0() {
//     let p = program!(~lets![
//        ~ "x" <- bern!(1/3);
//        ~ "y" <- var!(~ "x") ;
//        ~~ var!(~ "x")
//     ]);
//     check_approx1("let binding", 1.0 / 3.0, &p, 15000); // done!
//     println!("program1 pass");
//     let p = program!(~lets![
//        ~ "x" <- bern!(1/3);
//        ~ "y" <- bern!(1/3);
//        ~~ ite!(~ (b!(~@anf true) ) ? ( var!(~ "x") ) : ( var!(~ "y") ) )
//     ]);
//     check_approx1("let binding", 1.0 / 3.0, &p, 15000);

//     println!("program2 pass");
//     let p = program!(~lets![
//        ~ "x" <- exact!(flip!(1/3)) ;
//        ~ "y" <- var!(~ "x") ;
//        ~~ var!(~ "x")
//     ]);
//     check_approx1("let binding", 1.0 / 3.0, &p, 15000);
//     println!("program3 pass");
// }

// /// https://github.com/uiuc-arc/AQUA/blob/master/benchmarks/stan_bench/tug/tug.stan
// #[test]
// // #[traced_test]
// fn tug() {
//     let p = program!(~lets![
//         ~ "alice0" <- normal!(1.0, 1.0);
//         ~ "alice1" <- normal!(0.5, 0.5);
//         ~ "bob0" <- normal!(1.0, 1.0);
//         ~ "bob1" <- normal!(0.5, 0.5);

//         // unfortunately, this is only in sampling land. We could, perhaps, compile the following:
//         ~ "asplit" <- bern!(0.33);
//         ~ "bsplit" <- bern!(0.33);

//         ~ "alice" <- ite!(~ (var!(~ "asplit")) ? var!(~ "alice0") : var!(~ "alice1"));
//         ~ "bob"   <- ite!(~ (var!(~ "bsplit")) ? var!(~ "bob0") : var!(~ "bob1"));
//         // ... but we would have to deal with free variables (which is not really MLS friendly)
//         // the rest of the program requires observing in sampling language
//         // for x in xs {
//         //   observe: var!(~ "alice") > var!(~ "bob") == x
//         // }
//     ]);
// }

// /// modified program H from https://dl.acm.org/doi/pdf/10.1145/3490421 but just stubbing out some syntax
// #[test]
// // #[traced_test]
// fn gorinova_4_33_modified() {
//     let p = program!(~lets![
//         // ~ "phi0" <- myvec![0.3, 0.4, 0.5]
//         // ~ "phi1" <- myvec![0.4, 0.5, 0.3]
//         // ~ "phi2" <- myvec![0.5, 0.3, 0.4]
//         // ~ "mu"   <- myvec![0.3, 0.6, 0.9]
//            ~ "z1" <- categorical!(0.3, 0.4, 0.5); // phi0
//         // ~ "z23" <- ite!(~
//         //        (gt!(var!(~ "z1"), sval!(2))) ?
//         //             (lets![
//         //             ~ "z2_t" <- categorical!(prj!(~ "phi", var!("z1")));
//         //             ~ "z3_t" <- categorical!(prj!(~ "phi", var!("z2")));
//         //             ~~ var!(~ "z2_t", "z2_t")
//         //             ]) :
//         //            (lets![
//         //             ~ "z2_t" <- categorical!(prj!(~ "phi", var!("z1")));
//         //             ~ "z3_t" <- categorical!(prj!(~ "phi", var!("z1")));
//         //             ~~ var!(~ "z2_t", "z2_t")
//         //            ])
//         // );
//         // observe: var!(~ "y1"), normal!(prj!(~ "mu", var!(~ "z1")));
//         // observe: var!(~ "y2"), normal!(prj!(~ "mu", var!(~ "z2")));
//         // observe: var!(~ "y3"), normal!(prj!(~ "mu", var!(~ "z3")));
//     ]);
// }

// TODO: inspiration from https://dl.acm.org/doi/pdf/10.1145/3490421 a causal query could be nice

#[rustfmt::skip]
pub mod networks {
    // /// https://www.bnlearn.com/bnrepository/clgaussian-small.html#healthcare
    // pub fn healthcare() -> ProgramInferable {
    //    program!(~lets![
    //      ~ "Age"             <- categorical!(0.35, 0.45, 0.20); // A: young, adult, old
    //      ~ "PreExistingCond" <-                           // none, mild, severe
    //          ite!(~ (eq!(var!(~ "Age"), 0)) ? ( categorical!(0.88, 0.10, 0.20) ) : (
    //                 (eq!(var!(~ "Age"), 1)) ? ( categorical!(0.75, 0.20, 0.05) )
    //                                         : ( categorical!(0.42, 0.53, 0.05) )   ))
    //      ~ "OutpatientExpenditure" <-
    //          ite!(~ (eq!(var!(~ "Age"), 0)) ? ( normal!( 60.0,  100.0) ) : (
    //                 (eq!(var!(~ "Age"), 1)) ? ( normal!(180.0,  400.0) )
    //                                         : ( normal!(360.0, 1600.0) )   ))
    //      ~ "AnyHospitalStay" <-                    // any   (vs none)
    //          ite!(~ (eq!(var!(~ "Age"), 0)) ? ( bern!(0.1 ) ) : (
    //                 (eq!(var!(~ "Age"), 1)) ? ( bern!(0.25) )
    //                                         : ( bern!(0.40) )   ))
    //      ~ "DaysHospitalStay" <-
    //          ite!(~ (not!(~ "AnyHospitalStay")) ? ( 0.0 ) : (
    //                    ite!(~ (eq!(var!(~ "Age"), 0)) ? ( normal!( 1.0, 0.25) ) : (
    //                           (eq!(var!(~ "Age"), 1)) ? ( normal!( 4.0, 1.00) )
    //                                                   : ( normal!( 7.0, 2.25) )   ))
    //              ))
    //      ~ "InpatientExpenditure" <-
    //                    ite!(~ (eq!(var!(~ "PreExistingCond"), 0)) ? ( normal!( plus!(100, mult!(300, var!(~ "DaysHospitalStay"))),  30^2) ) : (
    //                           (eq!(var!(~ "PreExistingCond"), 1)) ? ( normal!( plus!(100, mult!(550, var!(~ "DaysHospitalStay"))),  50^2) )
    //                                                               : ( normal!( plus!(100, mult!(800, var!(~ "DaysHospitalStay"))), 100^2) )   ))
    //      ~ "Taxes" <- normal!(
    //          plus!(120,
    //                mult!(1.02, var!(~ "InpatientExpenditure")),
    //                mult!(1.05, var!(~ "OutpatientExpenditure"))
    //          ),  10^2)
    //      ~~ var!(~ "Taxes")
    //    ])
    // }
}

#[test]
// #[traced_test]
fn healthcare() {}

#[test]
fn arrival_router() {
    let mk = || {
        r#"
exact fn diamond (s1: Bool) -> Bool {
  let route = flip 0.5 in
  let s2 = if route then s1 else false in
  let s3 = if route then false else s1 in
  let drop = flip 0.0001 in
  s2 || (s3 && !drop)
}
sample {
  p ~ poisson(0.4);
  exact (iterate(diamond, true, p + 1))
}"#
    };

    let n = 100;

    let _ = crate::inference::importance_weighting_h(1, mk(), &Default::default());
    // crate::tests::check_approx("proto_arrival", vec![1.0 / 3.0, 1.0 / 3.0], mk(), n);
}

#[test]
// #[traced_test]
fn beta_3_1_flip_returns_true() {
    // notably, this will not work if extra samples are passed through the boundary and are unused. See (1)
    let mk = || {
        r#"sample {
      p ~ beta(1.0, 1.0);
      _ <- exact {
    "#.to_owned() + &(0..10).map(|i| {
        let obs = |d, ix| format!("let d{}_{} = flip p in observe d{}_{} == {} in ", i, ix, i, ix, d);
        let observes : [String; 4] = [obs(true, 0), obs(true, 1), obs(true, 2), obs(false, 3)];
        observes.into_iter().collect_vec().join("\n").to_owned()
    }).collect_vec().join("\n") +
        // (1) here is the return of our exact program. It must not produce samples which effect the weight. without needing to be pruned.
        " true\n    };" +
    r#"
      p
    }"#
    };
    let (n, s) = (1_000, 1);
    let seed = Some(s);
    check_approx_h("beta_3_1_flip", vec![3.0 / 4.0], &mk(), n, seed);
}

#[test]
fn beta_2_2_flip_followed_by_1_3_or_3_1() {
    // x = flips(0.5, 20)
    // y = conditional_flips((1./4, 3./4), x)
    let data = [
        (true, false),
        (true, true),
        (false, true),
        (true, false),
        (false, true),
        (true, false),
        (true, true),
        (false, true),
        (true, false),
        (true, true),
        (true, false),
        (true, false),
        (true, false),
        (false, true),
        (true, false),
        (true, false),
        (false, false),
        (false, true),
        (false, true),
        (true, false),
        (false, true),
        (true, false),
        (false, false),
        (true, false),
        (true, false),
        (true, false),
        (true, false),
        (false, true),
        (true, false),
        (false, true),
        (false, true),
        (true, false),
        (false, false),
        (true, true),
        (true, false),
        (true, false),
        (false, false),
        (true, false),
        (true, false),
        (false, false),
        (true, false),
        (true, false),
        (false, true),
        (false, true),
        (true, false),
        (false, true),
        (false, false),
        (false, false),
        (false, true),
        (true, true),
        (false, false),
        (false, true),
        (true, false),
        (true, false),
    ];
    let mk = || {
        r#"sample {
      p  ~ beta(1.0, 1.0);
      pt ~ beta(1.0, 1.0);
      pf ~ beta(1.0, 1.0);
    "#.to_owned() + &(0..3).map(|i| {
        let ix = |j| format!("{}_{}", i, j);
        let obs = |j, dx, dy| format!(r#"
      s{j} <- exact ( let x{j} = flip p in observe x{j} == {dx} in x{j} );
      _{j} <- exact ( let y{j} = if s{j} then flip pt else flip pf in observe y{j} == {dy} in true );
        "#, j=j, dx=dx, dy=dy);
        let observes : Vec<String> = data.iter().enumerate().map(|(j, (dx, dy))| obs(ix(j), dx, dy)).collect_vec();
        observes.join("").to_owned()
    }).collect_vec().join("\n") +
    r#"
      (p, pt, pf)
    }"#
    };
    let (n, s) = (100, 1);
    let seed = Some(s);
    check_approx_h(
        "beta_2_2_flip_followed_by_1_3_or_3_1",
        vec![0.60, 0.19, 0.63], // this is good enough for the test
        &mk(),
        n,
        seed,
    );
}
