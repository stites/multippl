use crate::compile::*;
use crate::grammar::*;
use crate::inference::*;
use crate::render::*;
use crate::typecheck::grammar::*;
use crate::*;
use itertools::*;
use rsdd::sample::probability::*;
use std::any::{Any, TypeId};
use std::ops::Range;
use tracing::*;
use tracing_test::*;

pub fn check_invariant(s: &str, precision: Option<f64>, n: Option<usize>, p: &ProgramTyped) {
    let precision = precision.unwrap_or_else(|| 0.01);
    let n = n.unwrap_or_else(|| 10000);
    let mut env_args = EnvArgs::default_args(None);
    let mut env = Env::from_args(&mut env_args);
    let exact = exact_inf(&mut env, &p.strip_samples());
    let approx = importance_weighting_inf(&mut env, n, p);
    assert_eq!(
        exact.len(),
        approx.len(),
        "[check_inv][{s}][mismatch shape] compiled exact queries {}, but approx returned results {}",
        renderfloats(&exact, false),
        renderfloats(&approx, false),
    );
    izip!(exact, approx)
        .enumerate()
        .for_each(|(i, (ext, apx))| {
            let ret = (ext - apx).abs() < precision;
            let i = i + 1;
            assert!(
                ret,
                "[check_inv][{s}#{i}][err]((exact: {ext}) - (approx: {apx})).abs < {precision}"
            );
        });
}
pub fn check_inference(
    infname: &str,
    inf: &dyn Fn(&mut Env, &ProgramTyped) -> Vec<f64>,
    precision: f64,
    s: &str,
    fs: Vec<f64>,
    p: &ProgramTyped,
) {
    let mut env_args = EnvArgs::default_args(None);
    let mut env = Env::from_args(&mut env_args);
    let prs = inf(&mut env, p);
    assert_eq!(
        prs.len(),
        fs.len(),
        "[check_{infname}][{s}] check_inference compiled queries {}, tests expect results {}",
        renderfloats(&prs, false),
        renderfloats(&fs, false),
    );
    debug!("query: {:?}", p.query());
    debug!("expecting: {}", renderfloats(&fs, false));
    debug!("computed:  {}", renderfloats(&prs, false));
    izip!(prs, fs).enumerate().for_each(|(i, (pr, f))| {
        let ret = (f - pr).abs() < precision;
        let i = i + 1;
        assert!(
            ret,
            "[check_{infname}][{s}#{i}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
        );
    });
}
pub fn check_exact(s: &str, f: Vec<f64>, p: &ProgramTyped) {
    let p = p.strip_samples();
    debug!("program: {:#?}", &p);
    check_inference("exact", &exact_inf, 0.000001, s, f, &p);
}
pub fn check_exact1(s: &str, f: f64, p: &ProgramTyped) {
    check_exact(s, vec![f], p)
}
pub fn check_approx(s: &str, f: Vec<f64>, p: &ProgramTyped, n: usize) {
    check_inference(
        "approx",
        &|env, p| importance_weighting_inf(env, n, p),
        0.01,
        s,
        f,
        p,
    );
}
pub fn check_approx1(s: &str, f: f64, p: &ProgramTyped, n: usize) {
    check_approx(s, vec![f], p, n)
}
pub fn check_approx_conc(s: &str, f: Vec<f64>, p: &ProgramTyped, n: usize) {
    let env_args = EnvArgs::default_args(None);
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
pub fn check_approx1_conc(s: &str, f: f64, p: &ProgramTyped, n: usize) {
    check_approx_conc(s, vec![f], p, n)
}
pub fn check_approx_seeded(s: &str, f: Vec<f64>, p: &ProgramTyped, n: usize, seeds: &Vec<u64>) {
    check_inference(
        "approx",
        &|env, p| importance_weighting_inf_seeded(seeds.clone(), n, p),
        0.01,
        s,
        f,
        p,
    );
}
pub fn check_approx_seeded1(s: &str, f: f64, p: &ProgramTyped, n: usize, seeds: &Vec<u64>) {
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
// #[traced_test]
fn program02() {
    let mk02 = |ret: ExprTyped| {
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
    let mk03 = |ret: ExprTyped| {
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
    let mk04 = |ret: ExprTyped| {
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
    let mk04 = |ret: ExprTyped| {
        Program::Body(lets![
            "x" ; B!() ;= sample!(flip!(1/3));
            "y" ; B!() ;= flip!(1/4);
            "_" ; B!() ;= observe!(b!("x" || "y"));
            ...? ret ; B!()
        ])
    };
    check_approx(
        "p04",
        vec![3.0 / 6.0, 4.0 / 6.0, 6.0 / 6.0, 1.0 / 6.0],
        &mk04(q!("y" x "x")),
        10000,
    );
    // check_approx1("p04/x|y", 6.0 / 6.0, &mk04(b!("x" || "y")), 10000);
    // check_approx1("p04/x&y", 1.0 / 6.0, &mk04(b!("x" && "y")), 10000);

    check_invariant("p04", None, None, &mk04(q!("y" x "x")));
    // check_invariant("p04/x  ", None, None, &mk04(b!("x")));
    // check_invariant("p04/x|y", None, None, &mk04(b!("x" || "y")));
    // check_invariant("p04/x&y", None, None, &mk04(b!("x" && "y")));
}

#[test]
// #[traced_test]
fn tuple0() {
    let p = {
        Program::Body(lets![
            "y" ; b!(B)   ;= b!(true);
            ...? b!("y", true) ; b!(B, B)
        ])
    };
    check_exact("tuples0/T,T", vec![1.0, 1.0], &p);
    let p = {
        Program::Body(lets![
            "y" ; b!(B)   ;= b!(true);
            "z" ; b!(B,B) ;= b!("y", true);
            ...? b!("z"; b!(B,B)) ; b!(B, B)
        ])
    };
    check_exact("tuples0/T,T", vec![1.0, 1.0], &p);
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "y" ; b!(B)   ;= b!(true);
            "z" ; b!(B,B) ;= b!("y", true);
            ...? ret ; B!()
        ])
    };
    check_exact("tuples0/T, ", vec![1.0], &mk(fst!(b!(@anf "z"))));
    check_exact("tuples0/ ,T", vec![1.0], &mk(snd!(b!(@anf "z"))));
}

#[test]
// #[traced_test]
fn tuple1() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "x" ; b!()     ;= flip!(1.0/3.0);
            "y" ; b!()     ;= flip!(1.0/4.0);
            "z" ; b!(B, B) ;= b!("x", "y");
            ...? ret ; B!()
        ])
    };
    check_exact("tuple1/x,y", vec![1.0 / 3.0, 1.0 / 4.0], &mk(b!("x", "y")));
    check_exact("tuple1/y,x", vec![1.0 / 4.0, 1.0 / 3.0], &mk(b!("y", "x")));

    check_exact("tuple1/x, ", vec![1.0 / 3.0], &mk(fst!(b!(@anf "z"))));
    check_exact("tuple1/ ,y", vec![1.0 / 4.0], &mk(snd!(b!(@anf "z"))));
}
#[test]
// #[traced_test]
fn sample_tuple() {
    let p = Program::Body(lets![
        "z" ; b!(B, B) ;= sample!(lets![
              "l" ; b!() ;= flip!(1/3);
              "r" ; b!() ;= flip!(1/4);
              ...? b!("l", "r") ; b!(B, B)
        ]);
       ...? b!("z" ; b!(B, B)); b!(B, B)
    ]);
    check_approx("sharedtuple", vec![1.0 / 3.0, 1.0 / 4.0], &p, 10000);
    check_invariant("sharedtuple ", None, None, &p);
}

// ===================================================================== //
//                          free variable tests                          //
// ===================================================================== //
#[test]
fn free_variables_0() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
           "x" ; B!() ;= flip!(1/3);
           "y" ; B!() ;= sample!(var!("x"));
           ...? ret ; B!()
        ])
    };
    let n = 10000;

    check_approx1("free/x ", 1.0 / 3.0, &mk(var!("x")), n);
    check_approx1("free/y ", 1.0 / 3.0, &mk(var!("y")), n);

    check_invariant("free/y ", None, None, &mk(var!("y")));
    check_invariant("free/x ", None, None, &mk(var!("x")));
}
#[test]
fn free_variables_1() {
    let problem = {
        Program::Body(lets![
           "x" ; B!() ;= flip!(1/3);
           "l" ; B!() ;= sample!(var!("x"));
           "_" ; B!() ;= observe!(var!("x"));
           ...? var!("l") ; B!()
        ])
    };
    check_approx1("free/!!", 1.0 / 3.0, &problem, 10000);
}

// ===================================================================== //
//                   START: deterministic if-then-else                   //
// ===================================================================== //

#[test]
// #[traced_test]
fn ite_00() {
    let mk = |p: ExprTyped| {
        Program::Body(ite!(
                    if ( p )
                    then { flip!(1/3) }
                    else { flip!(1/5) }))
    };
    check_exact1("ite_00/T", 1.0 / 3.0, &mk(b!(true)));
    check_exact1("ite_00/F", 1.0 / 5.0, &mk(b!(false)));
}

#[test]
// #[traced_test]
fn ite_0() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "b" ; b!() ;= ite!(
                if ( b!(true) )
                then { flip!(1/4) }
                else { flip!(1/5) });
            ...? ret ; b!()
        ])
    };
    check_exact1("ite_0  ", 1.0 / 4.0, &mk(var!("b")));
}

#[test]
fn ite_1() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "x" ; b!() ;= flip!(1/3);
            "y" ; b!() ;= ite!(
                if ( var!("x") )
                then { flip!(1/4) }
                else { flip!(2/5) });
            ...? ret ; b!()
        ])
    };
    let yres = (1.0 / 3.0 * 0.25) + (2.0 / 3.0 * 0.4);
    check_exact1("ite_1/x  ", 1.0 / 3.0, &mk(var!("x")));
    check_exact1("ite_1/y  ", yres, &mk(var!("y")));
    check_exact1("ite_1/x|y", 0.6, &mk(b!("x" || "y")));
    check_exact1("ite_1/x&y", 0.083333333, &mk(b!("x" && "y")));
}

#[test]
// #[traced_test]
// #[ignore]
fn ite_2() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "x" ; b!() ;= flip!(1/3);
            "y" ; b!() ;= ite!(
                if ( var!("x") )
                then { flip!(1/4) }
                else { flip!(1/5) });
            "_" ; b!() ;= observe!(b!("x" || "y"));
            ...? ret ; b!()
        ])
    };
    check_exact1("ite_2/y  ", 0.464285714, &mk(b!("y")));
    check_exact1("ite_2/x  ", 0.714285714, &mk(b!("x")));
    check_exact1("ite_2/x|y", 1.000000000, &mk(b!("x" || "y")));
    check_exact1("ite_2/x&y", 0.178571429, &mk(b!("x" && "y")));
}

#[test]
fn ite_3_with_one_sample_easy() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "x" ; b!() ;= flip!(1/3);
            "y" ; b!() ;= ite!(
                if ( var!("x") )
                then { sample!(flip!(1/4)) }
                else { flip!(1/5) });
            "_" ; b!() ;= observe!(b!("x" || "y"));
            ...? ret ; b!()
        ])
    };
    let n = 1000;
    check_approx1("ite_3/x  ", 0.714285714, &mk(b!("x")), n);
    check_approx1("ite_3/x|y", 1.000000000, &mk(b!("x" || "y")), n);
}

// ============================================================ //
// nested tests
// ============================================================ //
#[test]
fn nested_1() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "x" ; B!() ;= sample!(sample!(flip!(1/3)));
            "y" ; B!() ;= flip!(1/4);
            "_" ; B!() ;= observe!(b!("x" || "y"));
            ...? ret ; B!()
        ])
    };
    check_approx1("nest_1/y  ", 3.0 / 6.0, &mk(b!("y")), 10000);
    check_approx1("nest_1/x  ", 4.0 / 6.0, &mk(b!("x")), 10000);
    check_approx1("nest_1/x|y", 6.0 / 6.0, &mk(b!("x" || "y")), 10000);
    check_approx1("nest_1/x&y", 1.0 / 6.0, &mk(b!("x" && "y")), 10000);
    check_invariant("nest_1/x&y", None, None, &mk(b!("x" && "y")));
}

#[test]
fn nested_2() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "x" ; B!() ;= flip!(2/5);
            "y" ; B!() ;= sample!(
                lets![
                    "x1" ; B!() ;= sample!(flip!(1/3));
                    "y1" ; B!() ;= flip!(1/4);
                    ...? b!("x1" || "y1"); B!()
                ]);
            "_" ; B!() ;= observe!(b!("x" || "y")); // is this a problem?
            ...? ret ; B!()
        ])
    };
    check_exact1("nest_2/exact/y  ", 0.714285714, &mk(b!("y")));
    check_exact1("nest_2/exact/x  ", 0.571428571, &mk(b!("x")));
    check_exact1("nest_2/exact/x|y", 1.000000000, &mk(b!("x" || "y")));
    check_exact1("nest_2/exact/x&y", 0.285714286, &mk(b!("x" && "y")));

    let n = 10000;
    check_approx1("nest_2/appx/y  ", 0.714285714, &mk(b!("y")), n);
    check_approx1("nest_2/appx/x  ", 0.571428571, &mk(b!("x")), n);
    check_approx1("nest_2/appx/x|y", 1.000000000, &mk(b!("x" || "y")), n);
    check_approx1("nest_2/appx/x&y", 0.285714286, &mk(b!("x" && "y")), n);

    let n = Some(n);
    check_invariant("nest_2/invt/y  ", None, n, &mk(b!("y")));
    check_invariant("nest_2/invt/x  ", None, n, &mk(b!("x")));
    check_invariant("nest_2/invt/x|y", None, n, &mk(b!("x" || "y")));
    check_invariant("nest_2/invt/x&y", None, n, &mk(b!("x" && "y")));
}

// ============================================================ //
// grid tests
// ============================================================ //

#[test]
// #[traced_test]
fn grid2x2_warmup0() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "00" ; B!() ;= flip!(1/2);
            "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            ...? ret ; B!()
        ])
    };
    check_exact1("grid2x2/0/00", 1.0 / 2.0, &mk(b!("00")));
    check_exact1("grid2x2/0/01", 0.291666667, &mk(b!("01")));
    check_exact1("grid2x2/0/10", 0.183333333, &mk(b!("10")));
}
#[test]
// #[traced_test]
// #[ignore]
fn grid2x2_warmup1() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "01" ; B!() ;= flip!(1/3) ;
            "10" ; B!() ;= flip!(1/4) ;
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(3/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(3/9) ) : (
                                                          flip!(3/11) ))))));
            ...? ret ; B!()
        ])
    };
    check_exact1("grid2x2/2/11", 0.317911255, &mk(b!("11")));
}
/// a directed 2x2 grid test where we place samples according to various policies
///   (0,0) -> (0,1)
///     v        v
///   (1,0) -> (1,1)
#[test]
// #[ignore]
// #[traced_test]
fn grid2x2() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "00" ; B!() ;= flip!(1/2);
            "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                          flip!(1/11) ))))));
            ...? ret ; B!()
        ])
    };
    check_exact1("grid2x2/3/00", 1.0 / 2.0, &mk(b!("00")));
    check_exact1("grid2x2/3/01", 0.291666667, &mk(b!("01")));
    check_exact1("grid2x2/3/10", 0.183333333, &mk(b!("10")));
    check_exact1("grid2x2/3/11", 0.102927589, &mk(b!("11")));
}

#[test]
// #[ignore]
// #[traced_test]
fn grid2x2_sampled() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "00" ; B!() ;= flip!(1/2);
            "01_10" ; b!(B, B) ;= sample!(
                lets![
                    "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                    "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
                    ...? b!("01", "10") ; b!(B, B)
                ]);
            "01" ; B!() ;= fst!("01_10");
            "10" ; B!() ;= snd!("01_10");
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                          flip!(1/11) ))))));
            ...? ret ; B!()
        ])
    };
    check_approx1("grid2x2/approx_diag/00", 1.0 / 2.0, &mk(b!("00")), 10000);
    check_approx1("grid2x2/approx_diag/01", 0.291666667, &mk(b!("01")), 10000);
    check_approx1("grid2x2/approx_diag/10", 0.183333333, &mk(b!("10")), 10000);
    check_approx1("grid2x2/approx_diag/11", 0.102927589, &mk(b!("11")), 10000);
}

/// a directed 3x3 grid test where we place samples according to various policies
///   (0,0) -> (0,1) -> (0,2)
///     v        v        v
///   (1,0) -> (1,1) -> (1,2)
///     v        v        v
///   (2,0) -> (2,1) -> (2,2)
#[test]
// #[ignore]
// #[traced_test]
fn grid3x3_sampled_diag() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "00" ; B!() ;= flip!(1/2);
            "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );

            "20_11_02" ; b!(B, B) ;= sample!(
                lets![
                  "20" ; B!() ;= ite!( ( not!("10") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
                  "11" ; B!() ;=
                      ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                      ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                      ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                                flip!(1/11) ))))));
                  "02" ; B!() ;= ite!( ( b!(@anf "01")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                          ...? b!("20", "11", "02") ; b!(B, B, B)
                ]);
            "20" ; B!() ;= fst!("20_11_02");
            "11" ; B!() ;= snd!("20_11_02");
            "02" ; B!() ;= thd!("20_11_02");

            "21" ; B!() ;=
                ite!(( b!((  b!(@anf "20")) && (  b!(@anf "11"))) ) ? ( flip!(2/7) ) : (
                ite!(( b!((  b!(@anf "20")) && (not!("11"))) ) ? ( flip!(2/8) ) : (
                ite!(( b!((  not!("20")) && (  b!(@anf "11"))) ) ? ( flip!(2/9) ) : (
                                                          flip!(2/11) ))))));

            "12" ; B!() ;=
                ite!(( b!((  b!(@anf "11")) && (  b!(@anf "02"))) ) ? ( flip!(6/7) ) : (
                ite!(( b!((  b!(@anf "11")) && (not!("02"))) ) ? ( flip!(6/8) ) : (
                ite!(( b!((  not!("11")) && (  b!(@anf "02"))) ) ? ( flip!(6/9) ) : (
                                                          flip!(6/11) ))))));

            "22" ; B!() ;=
                ite!(( b!((  b!(@anf "21")) && (  b!(@anf "12"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "21")) && (not!("12"))) ) ? ( flip!(3/8) ) : (
                ite!(( b!((  not!("21")) && (  b!(@anf "12"))) ) ? ( flip!(8/9) ) : (
                                                          flip!(9/11) ))))));
            ...? ret ; B!()
        ])
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
        &mk(b!("00", "01", "10", "02", "20", "11", "12", "21", "22")),
        100000,
    );
}

/// a directed 3x3 grid test where we place samples according to various policies
///   (0,0) -> (0,1) -> (0,2)
///     v        v        v
///   (1,0) -> (1,1) -> (1,2)
///     v        v        v
///   (2,0) -> (2,1) -> (2,2)
#[test]
fn grid3x3() {
    let mk = |ret: ExprTyped| {
        Program::Body(lets![
            "00" ; B!() ;= flip!(1/2);
            "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "02" ; B!() ;= ite!( ( b!(@anf "01")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            "20" ; B!() ;= ite!( ( not!("10") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );

            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                          flip!(1/11) ))))));

            "21" ; B!() ;=
                ite!(( b!((  b!(@anf "20")) && (  b!(@anf "11"))) ) ? ( flip!(2/7) ) : (
                ite!(( b!((  b!(@anf "20")) && (not!("11"))) ) ? ( flip!(2/8) ) : (
                ite!(( b!((  not!("20")) && (  b!(@anf "11"))) ) ? ( flip!(2/9) ) : (
                                                          flip!(2/11) ))))));

            "12" ; B!() ;=
                ite!(( b!((  b!(@anf "11")) && (  b!(@anf "02"))) ) ? ( flip!(6/7) ) : (
                ite!(( b!((  b!(@anf "11")) && (not!("02"))) ) ? ( flip!(6/8) ) : (
                ite!(( b!((  not!("11")) && (  b!(@anf "02"))) ) ? ( flip!(6/9) ) : (
                                                          flip!(6/11) ))))));

            "22" ; B!() ;=
                ite!(( b!((  b!(@anf "21")) && (  b!(@anf "12"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "21")) && (not!("12"))) ) ? ( flip!(3/8) ) : (
                ite!(( b!((  not!("21")) && (  b!(@anf "12"))) ) ? ( flip!(8/9) ) : (
                                                          flip!(9/11) ))))));
            ...? ret ; B!()
        ])
    };
    check_exact1("grid3x3/exact/00", 0.500000000, &mk(b!("00")));
    check_exact1("grid3x3/exact/01", 0.291666667, &mk(b!("01")));
    check_exact1("grid3x3/exact/10", 0.183333333, &mk(b!("10")));
    check_exact1("grid3x3/exact/02", 0.274305556, &mk(b!("02")));
    check_exact1("grid3x3/exact/20", 0.193888889, &mk(b!("20")));
    check_exact1("grid3x3/exact/11", 0.102927589, &mk(b!("11")));
    check_exact1("grid3x3/exact/12", 0.599355085, &mk(b!("12")));
    check_exact1("grid3x3/exact/21", 0.199103758, &mk(b!("21")));
    check_exact1("grid3x3/exact/22", 0.770263904, &mk(b!("22")));
}
