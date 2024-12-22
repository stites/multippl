use crate::compile::*;
use crate::grammar::*;
use crate::inference::*;
use crate::tests::checks::*;
use crate::typeinf::grammar::*;
use crate::*;

use crate::data::HashMap;
use itertools::*;
use rsdd::sample::probability::*;
use std::any::{Any, TypeId};
use std::ops::Range;
use tracing::*;
use tracing_test::*;
// ===================================================================== //
//                   START: deterministic if-then-else                   //
// ===================================================================== //

#[test]
fn ite_00() {
    let mk = |guard: &str| {
        r#"exact {
        if ("#
            .to_owned()
            + guard
            + r#")
        then ( flip (1.0 /3.0 ) )
        else ( flip (1.0 /5.0 ) )
        "#
    };

    check_exact1("ite_00/T", 1.0 / 3.0, &mk("true"));
    check_exact1("ite_00/F", 1.0 / 5.0, &mk("false"));
}

#[test]
fn ite_0() {
    let mk = |ret: &str| {
        r#"exact {
        let b = if ( true )
                then flip 1.0 / 4.0
                else flip 1.0 / 5.0 in
        b"#
        .to_owned()
            + ""
            + r"\n}"
    };

    check_exact1("ite_0  ", 1.0 / 4.0, &mk("b"));
}

#[test]
fn ite_1() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 1.0 / 3.0 in
        let y = if ( x )
                then flip 1.0 / 4.0
                else flip 2.0 / 5.0 in
        "#
        .to_owned()
            + ret
            + r"\n}"
    };

    let yres = (1.0 / 3.0 * 0.25) + (2.0 / 3.0 * 0.4);
    check_exact1("ite_1/x  ", 1.0 / 3.0, &mk("x"));
    check_exact1("ite_1/y  ", yres, &mk("y"));
    check_exact1("ite_1/x|y", 0.6, &mk("x || y"));
    check_exact1("ite_1/x&y", 0.083333333, &mk("x && y"));
}

#[test]
fn ite_2() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 1.0 / 3.0 in
        let y = if ( x )
                then flip 1.0 / 4.0
                else flip 1.0 / 5.0 in
        observe x || y in
        "#
        .to_owned()
            + ret
            + r"\n}"
    };

    check_exact1("ite_2/y  ", 0.464285714, &mk("y"));
    check_exact1("ite_2/x  ", 0.714285714, &mk("x"));
    check_exact1("ite_2/x|y", 1.000000000, &mk("x || y"));
    check_exact1("ite_2/x&y", 0.178571429, &mk("x && y"));
}

#[test]
fn ite_3_with_one_sample_hard1_simplified_even_more_true() {
    let p = r#"
    exact { if true then sample ( ~bern(1.0/3.0) ) else flip 1.0 / 4.0 }
    "#;
    let n = 10000;
    check_approx1("ite_3/y-sample1/4-simpl", 1.0 / 3.0, p, n);
    // check_approx1("ite_3/y-sample1/4-simpl", 0.2, mk(b!("x")), n);
}

#[test]
fn ite_3_with_one_sample_hard1_simplified_even_more_false() {
    let p = r#"
    exact { if false then sample ( ~bern(1.0/3.0) ) else flip 1.0 / 4.0 }
    "#;
    let n = 5000;
    check_approx1("ite_3/y-sample1/4-simpl", 1.0 / 4.0, p, n);
    // check_approx1("ite_3/y-sample1/4-simpl", 0.2, mk(b!("x")), n);
}
#[test]
fn ite_3_with_one_sample_hard1_simplified_more() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 3.0 / 5.0 in
        let y = if x then sample ( ~ bern(1.0 / 3.0)) else flip 1.0 / 4.0 in
        "#
        .to_owned()
            + ret
            + r"\n}"
    };
    let n = 5000;
    check_approx1("ite_3/y", 0.3, &mk("y"), n); // broken!
}

#[test]
fn ite_3_with_one_sample_hard1_simplified() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 1.0 / 5.0 in
        let y = if x then sample { ~ bern(1.0 / 3.0)} else flip 1.0 / 4.0 in
        "#
        .to_owned()
            + ret
            + r"\n}"
    };
    let n = 5000;

    // check_exact1("ite_3/y-sample1/4-simpl", 0.266666667, mk(b!("y")));
    check_approx1("ite_3/y-sample1/4-simpl", 0.266666667, &mk("y"), n);
    // dice's answer for 2/4 @ sample site
    // check_approx1("ite_3/y-sample2/4  ", 0.545454545, mk(b!("y")), n);
    // dice's answer for 3/4 @ sample site
    // check_approx1("ite_3/y-sample3/4 ", 0.772727273, mk(b!("y")), n);

    // last one to tackle:
    // dice's answer for 1/4 @ sample site
    // check_approx1("ite_3/x&y", 0.227272727, mk(b!("x" && "y")), n * n * n);
}

#[test]
fn ite_3_with_one_sample_easy_x() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 2.0 / 3.0 in
        let y = if x then sample { ~ bern(1.0 / 4.0)} else flip 1.0 / 5.0 in
        observe ( x || y ) in
        "#
        .to_owned()
            + ret
            + r"\n}"
    };
    check_approx1("ite_3/x  ", 0.909090909, &mk("x"), 1000);
}

#[test]
fn ite_3_with_one_sample_hard1() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 2.0 / 3.0 in
        let y = if x then sample { ~ bern(1.0 / 4.0)} else flip 1.0 / 5.0 in
        observe ( x || y ) in
        "#
        .to_owned()
            + ret
            + r"\n}"
    };
    let n = 3000;
    check_approx1("ite_3/observe/x  ", 0.909090909, &mk("x"), n);
    check_approx1("ite_3/observe/y  ", 0.318181818, &mk("y"), n);
    check_approx1("ite_3/observe/x|y", 1.000000000, &mk("x || y"), n);
    check_approx1("ite_3/observe/x&y", 0.227272727, &mk("x && y"), n);
}

#[test]
fn ite_3_with_one_sample_easy_x_or_y() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 2.0 / 3.0 in
        let y = if x then sample { ~ bern(1.0 / 4.0) } else flip 1.0 / 5.0 in
        observe ( x || y ) in
        "#
        .to_owned()
            + ret
            + r"\n}"
    };
    check_approx1("ite_3/x|y", 1.000000000, &mk("x || y"), 1000);
}
