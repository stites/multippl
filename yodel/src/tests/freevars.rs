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

// ===================================================================== //
//                          free variable tests                          //
// ===================================================================== //

#[test]
#[traced_test]
fn free_variables_0() {
    let mk = |ret: &str| {
        r#"exact {
  let x = flip (1.0/3.0) in
  let y = x in
  "#
        .to_owned()
            + ret
            + "\n}"
    };
    let n = 1000;

    check_approx("free/x,y", vec![1.0 / 3.0, 1.0 / 3.0], &mk("(x, y)"), n);
    check_approx1("free/y ", 1.0 / 3.0, &mk("y"), n);

    // check_invariant("free/y,x", None, None, &mk(b!("y", "x")));
    // check_invariant("free/x ", None, None, &mk(var!("x")));
}

pub fn allmarg(x: &str, y: &str) -> String {
    format!("({x}, {y}, {x} || {y}, {x} && {y})")
}
#[test]
// #[traced_test]
fn free_variable_2_inv() {
    let mk = |ret: &str| {
        (r#"exact {
  let x = flip (1.0/3.0) in
  let y = (let x0 = flip 1.0 / 5.0 in x0 || x) in
  observe (x || y) in
  "#
        .to_owned()
            + ret
            + "\n}")
            .to_string()
    };
    let n = 500;
    (|p: String| check_invariant("free2/x*y ", None, None, &p))(mk(&allmarg("x", "y")));
    (|p: String| check_approx("free2/x*y", vec![0.714285714], &p, n))(mk(&"x"));
    (|p: String| check_approx("---------", vec![1.000000000], &p, n))(mk(&"y"));
    (|p: String| check_approx("---------", vec![0.714285714, 1.0, 1.0, 0.714285714], &p, n))(mk(
        &allmarg("x", "y"),
    ));
    // (|p| debug_approx("free2/x*y", vec![0.714285714, 1.0, 1.0, 0.714285714], &p, 5))(mk(
    //     q!("x" x "y"),
    // ));
}

#[test]
// #[traced_test]
fn free_variables_1() {
    let problem = r#"
    exact {
      let x = flip 1.0 / 3.0 in
      let l = sample { exact(x) } in
      observe x in
      l
    }"#
    .to_owned()
        + "\n}";
    check_approx1("free/!!", 1.0, &problem, 100);
    // check_approx1("free/!!", 1.0, &problem, 2);
}

#[test]
fn free_variable_2_exact() {
    let mk = |ret: &str| {
        r#"exact {
    let x = flip (1.0/3.0) in
    let x0 = x in
    let x1 = flip(0.2) in
    let y = x0 || x1 in
    observe x || y in
    "#
        .to_owned()
            + ret
            + "\n}"
    };
    check_exact(
        "free_2/x*y",
        vec![0.714285714, 1.0, 1.0, 0.714285714],
        &mk(&allmarg("x", "y")),
    );
}

// FIXME: make sanf do a variable lookup that requires sanf to be on the env
#[test]
fn free_variable_2_approx() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip (1.0/3.0) in
        let y = sample {
            x0 <- exact { x };
            x1 ~ bern (0.2);
            x0 || x1
        } in
        observe x || y in
        "#
        .to_owned()
            + ret
            + "\n}"
    };
    check_approx(
        "free2/x*y",
        vec![0.714285714, 1.0, 1.0, 0.714285714],
        &mk(&allmarg("x", "y")),
        10000,
    );
}

#[test]
fn free_variables_shared() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip (1.0/3.0) in
        let l = sample { exact { x } } in
        let r = sample { exact { x } } in
        "#
        .to_owned()
            + ret
            + "\n}"
    };
    check_approx(
        "shared_all",
        vec![1.0 / 3.0; 4],
        &mk(&allmarg("l", "r")),
        10000,
    );
}

#[test]
//#[traced_test]
fn free_variables_shared_tuple() {
    let mk = || {
        r#"exact {
            let x = flip (1.0/3.0) in
            let z = sample {
                lft <- exact ( x );
                rgt <- exact ( x );
                (lft, rgt)
            } in z
        }
        "#
    };

    check_approx("sharedtuple", vec![1.0 / 3.0, 1.0 / 3.0], &mk(), 15000);
}
#[test]
#[ignore]
fn free_variable_2_approx_again() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 1.0 / 3.0 in
        let y = sample { x0 ~ bern 1.0 / 5.0; x <- exact ( x ); x0 || x } in
        observe ( x || y ) in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    check_approx(
        "free2/x*y",
        vec![0.714285714, 1.0, 1.0, 0.714285714],
        &mk(&allmarg("x", "y")),
        10000,
    );
    check_approx1("free2/x", 0.714285714, &mk("x"), 10000);
    check_approx1("free2/x&y", 0.714285714, &mk("x && y"), 10000);
}
