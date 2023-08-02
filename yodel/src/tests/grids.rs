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

// ============================================================ //
// grid tests
// ============================================================ //

#[test]
fn grid2x2_warmup0() {
    let mk = |ret: &str| {
        r#"exact {
        let x00 = flip 1.0 / 2.0 in
        let x01 = if x00 then flip 1.0 / 3.0 else flip 1.0 / 4.0 in
        let x10 = if !x00 then flip 1.0 / 5.0 else flip 1.0 / 6.0 in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    check_exact1("grid2x2/0/00", 1.0 / 2.0, &mk("x00"));
    check_exact1("grid2x2/0/01", 0.291666667, &mk("x01"));
    check_exact1("grid2x2/0/10", 0.183333333, &mk("x10"));
}

#[test]
fn grid2x2_warmup1() {
    let mk = |ret: &str| {
        r#"exact {
        let x01 = flip 1.0 / 3.0 in
        let x10 = flip 1.0 / 4.0 in
        let x11 =
            if  x10  &&  x01 then flip 3.0 / 7.0 else
            if  x10  && !x01 then flip 3.0 / 8.0 else
            if !x10  &&  x01 then flip 3.0 / 9.0 else
                                  flip 3.0 / 11.0 in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    check_exact1("grid2x2/2/11", 0.317911255, &mk("x11"));
}

/// a directed 2x2 grid test where we place samples according to various policies
///   (0,0) -> (0,1)
///     v        v
///   (1,0) -> (1,1)
#[test]
fn grid2x2() {
    let mk = |ret: &str| {
        r#"exact {
        let x00 = flip 1.0 / 2.0 in
        let x01 = if  x00 then flip 1.0 / 3.0 else flip 1.0 / 4.0 in
        let x10 = if !x00 then flip 1.0 / 5.0 else flip 1.0 / 6.0 in
        let x11 =
            if  x10  &&  x01 then flip 1.0 / 7.0 else
            if  x10  && !x01 then flip 1.0 / 8.0 else
            if !x10  &&  x01 then flip 1.0 / 9.0 else
                                  flip 1.0 / 11.0 in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    check_exact1("grid2x2/3/00", 0.500000000, &mk("x00"));
    check_exact1("grid2x2/3/01", 0.291666667, &mk("x01"));
    check_exact1("grid2x2/3/10", 0.183333333, &mk("x10"));
    check_exact1("grid2x2/3/11", 0.102927589, &mk("x11"));
}

#[test]
// #[traced_test]
fn grid2x2_sampled() {
    let mk = |ret: &str| {
        r#"exact {
        let x00 = flip 1.0 / 2.0 in
        let x01_10 = sample {
            x00 <- exact { x00 };
            p01 <- if  x00 then 1.0 / 3.0 else 1.0 / 4.0;
            p10 <- if !x00 then 1.0 / 5.0 else 1.0 / 6.0;
            x01 ~ bern(p01);
            x10 ~ bern(p10);
            (x01, x10)
        } in
        let x01 = x01_10[0] in
        let x10 = x01_10[1] in
        let x11 =
            if  x10  &&  x01 then flip 1.0 / 7.0 else
            if  x10  && !x01 then flip 1.0 / 8.0 else
            if !x10  &&  x01 then flip 1.0 / 9.0 else
                                  flip 1.0 / 11.0 in
        "#
        .to_owned()
            + ret
            + "}"
    };
    // check_approx1("grid2x2/approx_diag/00", 1.0 / 2.0, &mk(b!("00")), 10000);
    // check_approx1("grid2x2/approx_diag/01", 0.291666667, &mk(b!("01")), 10000);
    // check_approx1("grid2x2/approx_diag/10", 0.183333333, &mk(b!("10")), 10000);
    // check_approx1("grid2x2/approx_diag/11", 0.102927589, &mk(b!("11")), 10000);

    check_approx(
        "grid2x2/approx_diag/00,01,10,11",
        vec![1.0 / 2.0, 0.291666667, 0.183333333, 0.102927589],
        &mk("(x00, x01, x10, x11)"),
        5000,
    );
}

/// a directed 3x3 grid test where we place samples according to various policies
///   (0,0) -> (0,1) -> (0,2)
///     v        v        v
///   (1,0) -> (1,1) -> (1,2)
///     v        v        v
///   (2,0) -> (2,1) -> (2,2)
#[test]
#[cfg(features = "long_tests")]
fn grid3x3_sampled_diag() {
    let mk = |ret: &str| {
        r#"exact {
        let x00 = flip 1.0 / 2.0 in
        let x01 = if  x00 then flip 1.0 / 3.0  else flip 1.0 / 4.0 in
        let x10 = if !x00 then flip 1.0 / 5.0  else flip 1.0 / 6.0 in

        let x20_11_02 = sample {
            x20 ~ if !x10 then bern(1.0 / 5.0) else bern(1.0 / 6.0);
            x11 ~
                if   x10 &&   x01 then bern(1.0 /  7.0) else
                if   x10 && ! x01 then bern(1.0 /  8.0) else
                if ! x10 &&   x01 then bern(1.0 /  9.0) else
                                       bern(1.0 / 11.0);
            x02 ~ if x01 then bern(1.0 / 3.0) else bern(1.0 / 4.0);
            (x20, x11, x02)
        } in
        let x20 = x20_11_02[0] in
        let x11 = x20_11_02[1] in
        let x02 = x20_11_02[2] in

        let x21 =
            if  x20  &&  x11 then flip 2.0 /  7.0 else
            if  x20  && !x11 then flip 2.0 /  8.0 else
            if !x20  &&  x11 then flip 2.0 /  9.0 else
                                  flip 2.0 / 11.0 in
        let x12 =
            if  x11  &&  x02 then flip 6.0 /  7.0 else
            if  x11  && !x02 then flip 6.0 /  8.0 else
            if !x11  &&  x02 then flip 6.0 /  9.0 else
                                  flip 6.0 / 11.0 in
        let x22 =
            if  x21  &&  x12 then flip 3.0 /  7.0 else
            if  x21  && !x12 then flip 3.0 /  8.0 else
            if !x21  &&  x12 then flip 8.0 /  9.0 else
                                  flip 9.0 / 11.0 in
        "#
        .to_owned()
            + ret
            + "\n}"
    };
    // check_approx1("grid3x3/approx/00", 0.500000000, &mk(b!("00")), 10000);
    // check_approx1("grid3x3/approx/01", 0.291666667, &mk(b!("01")), 10000);
    // check_approx1("grid3x3/approx/10", 0.183333333, &mk(b!("10")), 10000);
    // check_approx1("grid3x3/approx/02", 0.274305556, &mk(b!("02")), 10000);
    // check_approx1("grid3x3/approx/20", 0.193888889, &mk(b!("20")), 10000);
    // check_approx1("grid3x3/approx/11", 0.102927589, &mk(b!("11")), 10000);
    // check_approx1("grid3x3/approx/12", 0.599355085, &mk(b!("12")), 10000);
    // check_approx1("grid3x3/approx/21", 0.199103758, &mk(b!("21")), 10000);
    // check_approx1("grid3x3/approx/22", 0.770263904, &mk(b!("22")), 10000);
    check_approx(
        "grid3x3/approx/[00,01,10,02,20,11,12,21,22]",
        vec![
            0.500000000,
            0.291666667,
            0.183333333,
            0.274305556,
            0.193888889,
            0.102927589,
            0.599355085,
            0.199103758,
            0.770263904,
        ],
        &mk("(x00, x01, x10, x02, x20, x11, x12, x21, x22)"),
        5000,
    );
}

/// a directed 3x3 grid test where we place samples according to various policies
///   (0,0) -> (0,1) -> (0,2)
///     v        v        v
///   (1,0) -> (1,1) -> (1,2)
///     v        v        v
///   (2,0) -> (2,1) -> (2,2)
#[test]
#[cfg(features = "long_tests")]
fn grid3x3() {
    let mk = |ret: &str| {
        r#"exact {
        let x00 = flip 1.0 / 2.0 in
        let x01 = if  x00 then flip 1.0 / 3.0  else flip 1.0 / 4.0 in
        let x02 = if  x01 then flip 1.0 / 3.0  else flip 1.0 / 4.0 in
        let x10 = if !x00 then flip 1.0 / 5.0  else flip 1.0 / 6.0 in
        let x20 = if !x10 then flip 1.0 / 5.0  else flip 1.0 / 6.0 in
        let x11 =
            if  x10  &&  x01 then flip 1.0 /  7.0 else
            if  x10  && !x01 then flip 1.0 /  8.0 else
            if !x10  &&  x01 then flip 1.0 /  9.0 else
                                  flip 1.0 / 11.0 in
        let x21 =
            if  x20  &&  x11 then flip 2.0 /  7.0 else
            if  x20  && !x11 then flip 2.0 /  8.0 else
            if !x20  &&  x11 then flip 2.0 /  9.0 else
                                  flip 2.0 / 11.0 in

        let x12 =
            if  x11  &&  x02 then flip 6.0 /  7.0 else
            if  x11  && !x02 then flip 6.0 /  8.0 else
            if !x11  &&  x02 then flip 6.0 /  9.0 else
                                  flip 6.0 / 11.0 in
        let x22 =
            if  x21  &&  x12 then flip 3.0 /  7.0 else
            if  x21  && !x12 then flip 3.0 /  8.0 else
            if !x21  &&  x12 then flip 8.0 /  9.0 else
                                  flip 9.0 / 11.0 in
        "#
        .to_owned()
            + ret
            + "\n}"
    };
    check_exact1("grid3x3/exact/00", 0.500000000, &mk("x00"));
    check_exact1("grid3x3/exact/01", 0.291666667, &mk("x01"));
    check_exact1("grid3x3/exact/10", 0.183333333, &mk("x10"));
    check_exact1("grid3x3/exact/02", 0.274305556, &mk("x02"));
    check_exact1("grid3x3/exact/20", 0.193888889, &mk("x20"));
    check_exact1("grid3x3/exact/11", 0.102927589, &mk("x11"));
    check_exact1("grid3x3/exact/12", 0.599355085, &mk("x12"));
    check_exact1("grid3x3/exact/21", 0.199103758, &mk("x21"));
    check_exact1("grid3x3/exact/22", 0.770263904, &mk("x22"));
}
