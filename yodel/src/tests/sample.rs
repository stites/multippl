use crate::compile::*;
use crate::grammar::*;
use crate::inference::*;
use crate::tests::checks::*;
use crate::typeinf::grammar::*;
use crate::*;

use crate::data::HashMap;
use itertools::*;
use rand::distributions::Bernoulli;
use rand::distributions::Distribution;
use rand::rngs::StdRng;
use rsdd::sample::probability::*;
use std::any::{Any, TypeId};
use std::ops::Range;
use tracing::*;
use tracing_test::*;

#[test]
fn constant() {
    check_approx_h("p00", vec![1.0], "sample { x <- true; x }", 5, Some(0));
}
#[test]
fn flip_1_3() {
    check_approx_h(
        "p00",
        vec![1.0 / 3.0],
        "sample { x ~ bern(1.0 / 3.0); x }",
        3,
        Some(0),
    );
}
#[test]
fn two_coins() {
    let mk02 = |ret: &str| {
        r#"sample {
    x ~ bern(1.0 / 3.0);
    y ~ bern(1.0 / 4.0);
    "#
        .to_owned()
            + ret
            + "\n}"
    };
    let n = 5000;
    let seed = Some(0);
    check_approx_h("p02/y  ", vec![3.0 / 12.0], &mk02("y"), n, seed);
    check_approx_h("p02/x  ", vec![4.0 / 12.0], &mk02("x"), n, seed);
    check_approx_h("p02/x|y", vec![6.0 / 12.0], &mk02("x || y"), n, seed);
    check_approx_h("p02/x&y", vec![1.0 / 12.0], &mk02("x && y"), n, seed);
}

#[test]
fn beta_3_1() {
    let mk = || {
        r#"sample {
      p ~ beta(1.0, 1.0);
    "#
        .to_owned()
            + &(0..20)
                .map(|_| {
                    r#"
      observe true  from bern(p);
      observe true  from bern(p);
      observe true  from bern(p);
      observe false from bern(p);
    "#
                    .to_owned()
                })
                .collect_vec()
                .join("\n")
            + "p\n}"
    };
    let (n, s) = (10000, 0);
    let seed = Some(0);
    check_approx_h("beta_3_1", vec![3.0 / 4.0], &mk(), n, seed);
}

#[test]
fn beta_1_3() {
    let mk = || {
        r#"sample {
      p ~ beta(1.0, 1.0);
    "#
        .to_owned()
            + &(0..20)
                .map(|_| {
                    r#"
      observe true  from bern(p);
      observe false from bern(p);
      observe false from bern(p);
      observe false from bern(p);
    "#
                    .to_owned()
                })
                .collect_vec()
                .join("\n")
            + "p\n}"
    };
    let (n, s) = (10000, 0);
    let seed = Some(s);
    check_approx_h("beta_1_3", vec![1.0 / 4.0], &mk(), n, seed);
}
