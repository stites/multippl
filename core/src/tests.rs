use super::semantics::*;
use super::*;
use grammar::*;
use inference::*;
use std::any::{Any, TypeId};
use tracing_test::traced_test;

fn check_inference(
    i: &str,
    inf: &dyn Fn(&mut Env, &Program) -> Vec<f64>,
    precision: f64,
    s: &str,
    fs: Vec<f64>,
    p: &Program,
) {
    let mut env_args = EnvArgs::default_args(None);
    let mut env = Env::from_args(&mut env_args);
    let prs = inf(&mut env, p);
    assert_eq!(
        prs.len(),
        fs.len(),
        "check_inference compiled {} queries, tests expect {} results",
        prs.len(),
        fs.len(),
    );
    izip!(prs, fs).for_each(|(pr, f)| {
        let ret = (f - pr).abs() < precision;
        assert!(
            ret,
            "[check_{i}][{s}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
        );
    });
}
pub fn check_exact(s: &str, f: Vec<f64>, p: &Program) {
    check_inference("exact", &exact_inf, 0.000001, s, f, p);
}
pub fn check_exact1(s: &str, f: f64, p: &Program) {
    check_exact(s, vec![f], p)
}
pub fn check_approx(s: &str, f: Vec<f64>, p: &Program, n: usize) {
    check_inference(
        "approx",
        &|env, p| importance_weighting_inf(env, n, p),
        0.01,
        s,
        f,
        p,
    );
}
pub fn check_approx1(s: &str, f: f64, p: &Program, n: usize) {
    check_approx(s, vec![f], p, n)
}
pub fn check_approx_conc(s: &str, f: Vec<f64>, p: &Program, n: usize) {
    // check_inference(
    //     "approx",
    //     &|env, p| importance_weighting_inf_conc(env, n, p),
    //     0.01,
    //     s,
    //     f,
    //     p,
    // );
    let mut env_args = EnvArgs::default_args(None);
    // let mut env = Env::from_args(&mut env_args);
    let precision = 0.01;
    let fs = f;
    let i = "approx";
    let prs = importance_weighting_inf_conc(&env_args, n, p);
    assert_eq!(
        prs.len(),
        fs.len(),
        "check_inference compiled {} queries, tests expect {} results",
        prs.len(),
        fs.len(),
    );
    izip!(prs, fs).for_each(|(pr, f)| {
        let ret = (f - pr).abs() < precision;
        assert!(
            ret,
            "[check_{i}][{s}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
        );
    });
}
pub fn check_approx1_conc(s: &str, f: f64, p: &Program, n: usize) {
    check_approx_conc(s, vec![f], p, n)
}
pub fn check_approx_seeded(s: &str, f: Vec<f64>, p: &Program, n: usize, seeds: &Vec<u64>) {
    check_inference(
        "approx",
        &|env, p| importance_weighting_inf_seeded(seeds.clone(), n, p),
        0.01,
        s,
        f,
        p,
    );
}
pub fn check_approx_seeded1(s: &str, f: f64, p: &Program, n: usize, seeds: &Vec<u64>) {
    check_approx_seeded(s, vec![f], p, n, seeds)
}

#[test]
//#[traced_test]
fn program00() {
    let p00 = lets!["x" : bool := val!(true); in var!("x") ; bool];
    check_exact("p00", vec![1.0], &Program::Body(p00));
}
#[test]
// #[traced_test]
fn program01() {
    let p01 = lets![
        "x" ; B!() ;= flip!(1.0/3.0);
        ...? var!("x") ; B!()
    ];
    check_exact1("p01", 1.0 / 3.0, &Program::Body(p01));
}
#[test]
#[traced_test]
fn program02() {
    let mk02 = |ret: Expr| {
        Program::Body(lets![
            "x" ; B!() ;= flip!(1.0/3.0);
            "y" ; B!() ;= flip!(1.0/4.0);
            ...? ret ; B!()
        ])
    };
    check_exact1("p02/y  ", 3.0 / 12.0, &mk02(b!("y")));
    check_exact1("p02/x  ", 4.0 / 12.0, &mk02(b!("x")));
    check_exact1("p02/x|y", 6.0 / 12.0, &mk02(b!("x" || "y")));
    check_exact1("p02/x&y", 1.0 / 12.0, &mk02(b!("x" && "y")));
}

#[test]
// #[traced_test]
fn program03() {
    let mk03 = |ret: Expr| {
        Program::Body(lets![
            "x" ; B!() ;= flip!(1.0/3.0);
            "y" ; B!() ;= flip!(1.0/4.0);
            "_" ; B!() ;= observe!(b!("x" || "y"));
            ...? ret ; B!()
        ])
    };
    check_exact1("p03/y  ", 3.0 / 6.0, &mk03(b!("y")));
    check_exact1("p03/x  ", 4.0 / 6.0, &mk03(b!("x")));
    check_exact1("p03/x|y", 6.0 / 6.0, &mk03(b!("x" || "y")));
    check_exact1("p03/x&y", 1.0 / 6.0, &mk03(b!("x" && "y")));
}

#[test]
// #[traced_test]
fn program04_seeded() {
    let mk04 = |ret: Expr| {
        Program::Body(lets![
            "x" ; B!() ;= sample!(flip!(1/3));
            "y" ; B!() ;= flip!(1/4);
            "_" ; B!() ;= observe!(b!("x" || "y"));
            ...? ret   ; B!()
        ])
    };
    // perfect seeds of [F F T]
    let s = vec![1, 1, 7];
    let n = 1000;
    check_approx_seeded1("p04s/y  ", 3.0 / 6.0, &mk04(b!("y")), n, &s);
    check_approx_seeded1("p04s/x  ", 4.0 / 6.0, &mk04(b!("x")), n, &s);
    check_approx_seeded1("p04s/x|y", 6.0 / 6.0, &mk04(b!("x" || "y")), n, &s);
    check_approx_seeded1("p04s/x&y", 1.0 / 6.0, &mk04(b!("x" && "y")), n, &s);
}

#[test]
// #[traced_test]
fn program04_approx() {
    let mk04 = |ret: Expr| {
        Program::Body(lets![
            "x" ; B!() ;= sample!(flip!(1/3));
            "y" ; B!() ;= flip!(1/4);
            "_" ; B!() ;= observe!(b!("x" || "y"));
            ...? ret ; B!()
        ])
    };
    check_approx1("p04/y  ", 3.0 / 6.0, &mk04(b!("y")), 10000);
    check_approx1("p04/x  ", 4.0 / 6.0, &mk04(b!("x")), 10000);
    check_approx1("p04/x|y", 6.0 / 6.0, &mk04(b!("x" || "y")), 10000);
    check_approx1("p04/x&y", 1.0 / 6.0, &mk04(b!("x" && "y")), 10000);
}
