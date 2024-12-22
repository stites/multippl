#![allow(clippy::redundant_closure_call)]
use crate::compile::*;
use crate::data::HashMap;
use crate::grammar::*;
use crate::inference::*;
use crate::tests::checks::*;
use crate::typeinf::grammar::*;
use crate::*;

use itertools::*;
use rsdd::sample::probability::*;
use std::any::{Any, TypeId};
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
}

pub fn allmarg(x: &str, y: &str) -> String {
    format!("({x}, {y}, {x} || {y}, {x} && {y})")
}

#[test]
// #[traced_test]
fn free_variable_2_inv_small() {
    let mk = |ret: &str| {
        r#"exact {
  let x = flip (1.0/3.0) in
  let y = (let x0 = flip 1.0 / 2.0 in x0 || x) in
  observe (x || y) in
  "#
        .to_owned()
            + ret
            + "\n}"
    };
    check_approx_h("freevars2_inv", vec![0.5], &mk("x"), 3, Some(0));
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
    check_approx_h("free/!!", vec![1.0], &problem, 3, Some(0));
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
    check_approx_h(
        "free2/x*y",
        // vec![0.714285714, 1.0, 1.0, 0.714285714],
        // &mk(&allmarg("x", "y")),
        vec![0.714285714],
        &mk("x"),
        15,
        Some(2),
    );
}

// let x = flip (1/3)  in
// let y = flip (1/2)      in
// let _ = observe x || (x || y) in
// // P(x) 0.5
// // P(y) 0.75
#[test]
fn free_variable_2_approx_simple() {
    let mk = |ret: &str| {
        r#"exact {
    let x = sample { exact { flip(1.0/3.0) } } in
    let y = sample { exact { flip(1.0/2.0) } } in
    observe x || (x || y) in
    "#
        .to_owned()
            + ret
            + "\n}"
    };
    check_approx_h(
        // check_exact(
        "free_2/x*y",
        // vec![0.714285714, 1.0, 1.0, 0.714285714],
        // &mk(&allmarg("x", "(x || y)")),
        vec![0.5, 0.75],
        &mk("(x, y)"),
        6000,
        Some(1),
    );
}

#[test]
fn free_variables_shared_seq() {
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
    check_approx_h(
        "shared_all",
        vec![1.0 / 3.0; 4],
        &mk(&allmarg("l", "r")),
        3,
        Some(0),
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
    check_approx_h("sharedtuple", vec![1.0 / 3.0, 1.0 / 3.0], mk(), 3, Some(0));
}

#[test]
fn observe_with_free_var_sampled() {
    let mk = |ret: &str| {
        r#"sample {
        x ~ bern(1.0 / 3.0);
        y <- exact {
            let y = flip 1.0 / 4.0 in
            observe x || y in
            y
        };
        "#
        .to_owned()
            + ret
            + r"\n}"
    };

    // 𝔼[x = ⊤] =  (p=1/3) ∘ 𝕀[⊤ = ⊤] ∘ (w=1) + (p=2/3) 𝕀[⊥ = ⊤] (w=1/4)
    //            ---------------------------------------------------------  = 2/3
    //                        (1/3 ∘ 1) + (2/3 ∘ 1/4)
    //
    //
    // 𝔼[y = ⊤] =  (p=1/12) ∘ (w=1) ∘ 𝕀[x=⊤,y=⊤] +  (p=2/3) ∘ (w=1/4) ∘ 𝕀[x=⊥,y=⊤]
    //            ---------------------------------------------------------------------= 1/2
    //                   (p=1/12) ∘ (w=1) + (p=3/12) ∘ (w=1) + (p=2/3) ∘ (w=1/4)
    check_approx(
        "observe_with_free_var_sampled",
        vec![2.0 / 3.0, 1.0 / 2.0, 1.0, 1.0 / 6.0], // x y x|y x&y
        &mk(&allmarg("x", "y")),
        10000,
    );
}

#[test]
fn observe_with_free_var() {
    let mk = |ret: &str| {
        r#"sample {
        x ~ bern(1.0 / 3.0);
        _ <- exact {
            let y = flip 1.0 / 4.0 in
            observe x || y in
            true
        };
        "#
        .to_owned()
            + ret
            + r"\n}"
    };
    check_approx1(
        "observe_with_free_var",
        2.0 / 3.0, // x y x|y x&y
        &mk("x"),
        10000,
    );
}
