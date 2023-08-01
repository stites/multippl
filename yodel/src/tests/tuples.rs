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

#[test]
#[traced_test]
fn tuple0() {
    let p = r#"exact { let x = (false, true) in fst x }"#;
    check_exact("tuples0/F, ", vec![0.0], &p);
}
#[test]
#[traced_test]
fn tuple1() {
    let p = r#"exact { let y = true in (y, true) }"#;
    check_exact("tuples0/T,T", vec![1.0, 1.0], &p);
}
#[test]
#[traced_test]
fn tuple2() {
    let p = r#"exact {
    let y = true in
    let z = (y, true) in
    z"#
    .to_owned()
        + ""
        + "\n}";
    check_exact("tuples0/T,T", vec![1.0, 1.0], &p);
}

macro_rules! tuple_34_tests {
    ($($name:ident ($query:expr): $value:expr,)*) => {
    $(
        #[test]
        #[traced_test]
        fn $name() {
            let mk = |ret: &str| {
                r#"exact {
            let y = true in
            let z = (y, true) in
            "#
                .to_owned()
                    + ret
                    + "\n}"
            };
            // check_exact("tuples0/T, ", vec![1.0], &mk("fst z"));
            ($value)(mk($query));
        }
    )*
    }
}
tuple_34_tests! {
    tuple3 ("fst z"): (|p: String| check_exact("tuples0/T, ", vec![1.0], &p)),
    tuple4 ("snd z"): (|p: String| check_exact("tuples0/ ,T", vec![1.0], &p)),
}

#[test]
// #[traced_test]
fn tuple5() {
    let mk = |ret: &str| {
        r#"exact {
    let x = flip (1.0/3.0) in
    let y = flip (1.0/4.0) in
    let z = (x, y) in
    "#
        .to_owned()
            + ret
            + "\n}"
    };

    check_exact("tuple1/x,y", vec![1.0 / 3.0, 1.0 / 4.0], &mk("(x, y)"));
    check_exact("tuple1/y,x", vec![1.0 / 4.0, 1.0 / 3.0], &mk("(y, x)"));

    check_exact("tuple1/x, ", vec![1.0 / 3.0], &mk("fst z"));
    check_exact("tuple1/ ,y", vec![1.0 / 4.0], &mk("snd z"));
}

#[test]
fn sample_tuple() {
    let p = r#"
    exact {
      let z = sample {
        l ~ bern(1.0/3.0);
        r ~ bern(1.0/4.0);
        (l, r)
    } in z"#
        .to_owned()
        + "\n}";
    check_approx("sharedtuple", vec![1.0 / 3.0, 1.0 / 4.0], &p, 20000);
    // check_invariant("sharedtuple ", None, None, &p);
}

#[test]
fn test_big_tuple1() {
    let p = r#"
    exact {
      let a = flip 0.5 in
      let b = flip 0.5 in
      let c = flip 0.5 in
      let d = flip 0.5 in
      let e = flip 0.5 in
      let z = (a, b, c, d, e) in
      z
    } "#
    .to_owned()
        + "\n}";
    check_exact("3-tuple", vec![0.5, 0.5, 0.5, 0.5, 0.5], &p);
}

#[test]
fn test_big_tuple2() {
    let p = r#"
    exact {
      let a = flip 0.5 in
      let b = flip 0.5 in
      let c = flip 0.5 in
      let d = flip 0.5 in
      let e = flip 0.5 in
      let z = (a, b, c, d, e) in
      let q = z[0] in
      let r = z[1] in
      let s = z[2] in
      let t = z[3] in
      let u = z[4] in
      (q, r, s, t, u)
    }"#
    .to_owned()
        + "\n}";
    check_exact("3-tuple", vec![0.5, 0.5, 0.5, 0.5, 0.5], &p);
}
