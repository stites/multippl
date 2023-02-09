use super::semantics::*;
use super::*;
use grammar::*;
use inference::*;
use tracing_test::traced_test;

fn check_inference(
    i: &str,
    inf: &dyn Fn(&mut Env, &Program) -> f64,
    precision: f64,
    s: &str,
    f: f64,
    p: &Program,
) {
    let mut env_args = EnvArgs::default_args(None);
    let mut env = Env::from_args(&mut env_args);
    let pr = inf(&mut env, p);
    let ret = (f - pr).abs() < precision;
    assert!(
        ret,
        "[check_{i}][{s}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
    );
}
pub fn check_exact(s: &str, f: f64, p: &Program) {
    check_inference("exact", &exact_inf, 0.000001, s, f, p);
}
pub fn check_approx(s: &str, f: f64, p: &Program, n: usize) {
    check_inference(
        "approx",
        &|env, p| importance_weighting_inf(env, n, p),
        0.01,
        s,
        f,
        p,
    );
}
pub fn check_approx_seeded(s: &str, f: f64, p: &Program, n: usize, seeds: &Vec<u64>) {
    // check "perfect seeds" -- vec![1, 7]
    let i = "approx";
    let precision = 0.01;
    let pr = importance_weighting_inf_seeded(seeds.clone(), n, p);
    let ret = (f - pr).abs() < precision;
    assert!(
        ret,
        "[check_{i}][{s}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
    );
}
#[test]
// #[traced_test]
fn program00() {
    let p00 = lets![
        "x" := val!(true);
        ...? var!("x")
    ];
    check_exact("p00", 1.0, &Program::Body(p00));
}
#[test]
// #[traced_test]
fn program01() {
    let p01 = lets![
        "x" := flip!(1.0/3.0);
        ...? var!("x")
    ];
    check_exact("p01", 1.0 / 3.0, &Program::Body(p01));
}
#[test]
// #[traced_test]
fn program02() {
    let mk02 = |ret: Expr| {
        Program::Body(lets![
            "x" := flip!(1.0/3.0);
            "y" := flip!(1.0/4.0);
            ...? ret
        ])
    };
    check_exact("p02/y  ", 3.0 / 12.0, &mk02(var!("y")));
    check_exact("p02/x  ", 4.0 / 12.0, &mk02(var!("x")));
    check_exact("p02/x|y", 6.0 / 12.0, &mk02(or!("x", "y")));
    check_exact("p02/x&y", 1.0 / 12.0, &mk02(and!("x", "y")));
}

#[test]
// #[traced_test]
fn program03() {
    let mk03 = |ret: Expr| {
        Program::Body(lets![
            "x" := flip!(1.0/3.0);
            "y" := flip!(1.0/4.0);
            "_" := observe!(or!("x", "y"));
            ...? ret
        ])
    };
    check_exact("p03/y  ", 3.0 / 6.0, &mk03(var!("y")));
    check_exact("p03/x  ", 4.0 / 6.0, &mk03(var!("x")));
    check_exact("p03/x|y", 6.0 / 6.0, &mk03(or!("x", "y")));
    check_exact("p03/x&y", 1.0 / 6.0, &mk03(and!("x", "y")));
}

#[test]
// #[traced_test]
fn program04_seeded() {
    let mk04 = |ret: Expr| {
        Program::Body(lets![
            "x" := sample!(flip!(1/3));
            "y" := flip!(1/4);
            "_" := observe!(or!("x", "y"));
            ...? ret
        ])
    };
    // perfect seeds of [F F T]
    let s = vec![1, 1, 7];
    let n = 1000;
    check_approx_seeded("p04s/y  ", 3.0 / 6.0, &mk04(var!("y")), n, &s);
    check_approx_seeded("p04s/x  ", 4.0 / 6.0, &mk04(var!("x")), n, &s);
    check_approx_seeded("p04s/x|y", 6.0 / 6.0, &mk04(or!("x", "y")), n, &s);
    check_approx_seeded("p04s/x&y", 1.0 / 6.0, &mk04(and!("x", "y")), n, &s);
}

#[test]
// #[traced_test]
fn program04_approx() {
    let mk04 = |ret: Expr| {
        Program::Body(lets![
            "x" := sample!(flip!(1/3));
            "y" := flip!(1/4);
            "_" := observe!(or!("x", "y"));
            ...? ret
        ])
    };
    check_approx("p04/y  ", 3.0 / 6.0, &mk04(var!("y")), 10000);
    check_approx("p04/x  ", 4.0 / 6.0, &mk04(var!("x")), 10000);
    check_approx("p04/x|y", 6.0 / 6.0, &mk04(or!("x", "y")), 10000);
    check_approx("p04/x&y", 1.0 / 6.0, &mk04(and!("x", "y")), 10000);
}
