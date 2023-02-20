use crate::grammar::*;
use crate::inference::*;
use crate::render::*;
use crate::semantics::*;
use crate::*;
use itertools::*;
use quickcheck::*;
use rsdd::sample::probability::*;
use std::any::{Any, TypeId};
use std::ops::Range;
use tracing_test::*;

impl Arbitrary for Val {
    fn arbitrary(g: &mut Gen) -> Self {
        let x = g.choose(&[None, Some(true), Some(false), Some(true), Some(false)]);
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(Some(b)) => Val::Bool(*b),
            Some(None) => {
                // let arb = u8::arbitrary(g);
                // let len = (arb % 3) + 2; // only generate between 2-4 tuples
                // Val::Prod((0..len).map(|_i| Self::arbitrary(g)).collect_vec())
                // ...actually, just work with 2-tuples for now.
                Val::Prod((0..2).map(|_i| Self::arbitrary(g)).collect_vec())
            }
        }
    }
}

impl Arbitrary for ANF {
    fn arbitrary(g: &mut Gen) -> Self {
        let x = g.choose(&[0, 1, 2_u8]).copied();
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(0) => ANF::AVar(String::arbitrary(g), Box::new(Ty::Bool)),
            Some(1) => ANF::AVal(Val::arbitrary(g)),
            Some(2) => {
                let x = g.choose(&[0, 1, 2_u8]);
                match x {
                    None => panic!("impossible: choose vec has len > 0"),
                    Some(0) => ANF::And(Box::<ANF>::arbitrary(g), Box::<ANF>::arbitrary(g)),
                    Some(1) => ANF::Or(Box::<ANF>::arbitrary(g), Box::<ANF>::arbitrary(g)),
                    Some(2) => ANF::Neg(Box::<ANF>::arbitrary(g)),
                    _ => panic!("impossible"),
                }
            }
            _ => panic!("impossible"),
        }
    }
}

impl Arbitrary for Expr {
    fn arbitrary(g: &mut Gen) -> Self {
        let x = g.choose(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]).copied();
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(0) => Expr::EAnf(Box::<ANF>::arbitrary(g)),
            Some(1) => Expr::EFst(Box::<ANF>::arbitrary(g), Box::new(Ty::Bool)),
            Some(2) => Expr::ESnd(Box::<ANF>::arbitrary(g), Box::new(Ty::Bool)),
            Some(3) => Expr::EProd(
                (0..2).map(|_i| ANF::arbitrary(g)).collect_vec(),
                Box::new(Ty::Bool),
            ),
            Some(4) => {
                let var = String::arbitrary(g);
                let bind = Box::<Expr>::arbitrary(g);
                let body = Box::<Expr>::arbitrary(g);
                Expr::ELetIn(var, Box::new(Ty::Bool), bind, body, Box::new(Ty::Bool))
            }
            Some(5) => {
                let p = Box::<ANF>::arbitrary(g);
                let t = Box::<Expr>::arbitrary(g);
                let f = Box::<Expr>::arbitrary(g);
                Expr::EIte(p, t, f, Box::new(Ty::Bool))
            }
            Some(6) => {
                let r = u8::arbitrary(g); // 256
                Expr::EFlip(<u8 as Into<f64>>::into(r) / <u8 as Into<f64>>::into(u8::MAX))
            }
            Some(7) => Expr::EObserve(Box::<ANF>::arbitrary(g)),
            Some(8) => Expr::ESample(Box::<Expr>::arbitrary(g)),
            _ => panic!("impossible"),
        }
    }
}
impl Arbitrary for Program {
    fn arbitrary(g: &mut Gen) -> Self {
        Program::Body(Expr::arbitrary(g))
    }
}

pub fn check_invariant(s: &str, precision: Option<f64>, n: Option<usize>, p: &Program) {
    let precision = precision.unwrap_or_else(|| 0.01);
    let n = n.unwrap_or_else(|| 100000);
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
        "[check_{infname}][{s}] check_inference compiled queries {}, tests expect results {}",
        renderfloats(&prs, false),
        renderfloats(&fs, false),
    );
    izip!(prs, fs).enumerate().for_each(|(i, (pr, f))| {
        let ret = (f - pr).abs() < precision;
        let i = i + 1;
        assert!(
            ret,
            "[check_{infname}][{s}#{i}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
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
// #[traced_test]
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

    check_invariant("p04/y  ", None, None, &mk04(b!("y")));
    check_invariant("p04/x  ", None, None, &mk04(b!("x")));
    check_invariant("p04/x|y", None, None, &mk04(b!("x" || "y")));
    check_invariant("p04/x&y", None, None, &mk04(b!("x" && "y")));
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
    let mk = |ret: Expr| {
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
    let mk = |ret: Expr| {
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
//                   BEGIN: ignored free variable tests                  //
// ===================================================================== //

#[test]
#[ignore = "this test will always break until we are doing data flow analysis"]
#[traced_test]
fn free_variables_0() {
    let mk = |ret: Expr| {
        Program::Body(lets![
           "x" ; B!() ;= flip!(1/3);
           "y" ; B!() ;= sample!(var!("x"));
           ...? ret ; B!()
        ])
    };
    let n = 10;

    check_approx1("free/x ", 1.0 / 3.0, &mk(var!("x")), n);
    // FIXME: still broken! getting 1/2 instead of 1/3
    check_approx1("free/y ", 1.0 / 3.0, &mk(var!("y")), n);

    check_invariant("free/y ", None, None, &mk(var!("y")));
    check_invariant("free/y ", None, None, &mk(var!("x")));
}

#[test]
#[ignore = "This will be broken until we reintroduce code at FIXME(#1)."]
// #[traced_test]
fn free_variables_1() {
    let problem = {
        Program::Body(lets![
           "x" ; B!() ;= flip!(1/3);
           "l" ; B!() ;= sample!(var!("x"));
           "_" ; B!() ;= observe!(var!("x"));
           ...? var!("l") ; B!()
        ])
    };
    check_approx1("free/!!", 1.0 / 3.0, &problem, 10);
}
#[test]
// #[traced_test]
#[ignore = "punt till data flow analysis"]
fn free_variables_2() {
    let mk = |ret: Expr| {
        Program::Body(lets![
            "x" ; b!() ;= flip!(1/3);
            "l" ; b!() ;= sample!(
                lets![
                    "x0" ; b!() ;= flip!(1/5);
                    ...? b!("x0" || "x") ; b!()
                ]);
           "_" ; b!() ;= observe!(var!("x")); // is this a problem?
           ...? ret ; b!()
        ])
    };
    check_approx1("free/?1", 1.0 / 1.0, &mk(var!("x")), 1000);
    check_approx1("free/?2", 1.0 / 1.0, &mk(var!("l")), 1000); // FIXME: need to derive the right number for this one
}

// free variable edge case
#[test]
// #[traced_test]
#[ignore = "I suspect this is also funky (but gives a seemingly obscure result) because of free_variables_0 issue"]
fn free_variables_shared() {
    let p = Program::Body(lets![
       "x" ; b!() ;= flip!(1/3);
       "l" ; b!() ;= sample!(var!("x"));
       "r" ; b!() ;= sample!(var!("x"));
       ...? b!("l" && "r") ; b!()
    ]);
    check_approx1("shared ", 1.0 / 3.0, &p, 1000);
}

#[test]
#[ignore = "also broken due to same reason as free_variables_0"]
#[traced_test]
fn free_variables_shared_tuple() {
    let p = Program::Body(lets![
       "x" ; b!()     ;= flip!(1/3);
       "z" ; b!(B, B) ;= sample!(b!("x", "x"));
       ...? b!("z" ; b!(B, B)); b!(B, B)
    ]);
    check_approx("sharedtuple", vec![1.0 / 3.0, 1.0 / 3.0], &p, 1000);
}
// ===================================================================== //
//                    END: ignored free variable tests                   //
// ===================================================================== //
// ===================================================================== //
//                   START: deterministic if-then-else                   //
// ===================================================================== //

#[test]
// #[traced_test]
fn ite_00() {
    let mk = |p: Expr| {
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
    let mk = |ret: Expr| {
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
// #[traced_test]
// #[ignore]
fn ite_1() {
    let mk = |ret: Expr| {
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
    let mk = |ret: Expr| {
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
// #[traced_test]
fn grid2x2_warmup0() {
    let mk = |ret: Expr| {
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
    let mk = |ret: Expr| {
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
    let mk = |ret: Expr| {
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
