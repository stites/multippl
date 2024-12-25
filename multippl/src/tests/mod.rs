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

mod checks;
mod freevars;
mod grids;
mod ite;
mod mls;
mod properties;
// mod nested;
mod sample;
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
    observe x || y in
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
    observe x || y in
    "#
        .to_owned()
            + ret
            + "\n}"
    };

    let n = 3;
    let seed = Some(0);
    check_approx_h("p04s/y  ", vec![3.0 / 6.0], &mk("y"), n, seed);
    check_approx_h("p04s/x  ", vec![4.0 / 6.0], &mk("x"), n, seed);
    check_approx_h("p04s/x|y", vec![6.0 / 6.0], &mk("x || y"), n, seed);
    check_approx_h("p04s/x&y", vec![1.0 / 6.0], &mk("x && y"), n, seed);
    // check_invariant("p04", None, None, &mk(&allmarg("y", "x")));
}

#[test]
fn shadows() {
    let p = r#"
sample {
  x <- (x <- 1; 2);
  x
}
"#
    .to_owned();
    check_approx_h("let", vec![2.0], &p, 1, None);

    let p = r#"
sample fn foo (x: Int) -> Int {
  x + 1
}
sample {
  x <- foo(3);
  x
}
"#
    .to_owned();
    check_approx_h("fn", vec![4.0], &p, 1, None);

    let p = r#"
sample fn bar (x: Int) -> Int {
  x + 1
}
sample fn foo (x: Int) -> Int {
  x <- bar(x + 2);
  x + 3
}
sample {
  x <- foo(1);
  x
}
"#
    .to_owned();
    check_approx_h("double-fn", vec![7.0], &p, 1, None);

    let p = r#"
sample fn bar (x: Int) -> Int {
  x <- x + 1;
  x + 1
}
sample fn foo (x: Int) -> Int {
  x <- x + 1;
  bar(x + 1)
}
sample {
  x <- foo(3);
  x
}
"#
    .to_owned();
    check_approx_h("double-fn+let", vec![7.0], &p, 1, None);

    let p = r#"
sample fn bar (x: Int) -> Int {
  x + 1
}
exact fn foo (x: Int) -> Int {
  let x = sample(bar(x + 2)) in
  x + 3
}
sample {
  x <- exact(foo(1));
  x
}
"#
    .to_owned();
    check_approx_h("mls-double-fn", vec![7.0], &p, 1, None);
}
