use crate::compile::*;
use crate::grammar::*;
use crate::inference::*;
use crate::tests::checks::*;
use crate::typeinf::grammar::*;
use crate::*;

use crate::utils::render::renderfloats;
use core::fmt::Debug;
use itertools::*;
use num_rational::Rational32;
use num_traits::identities::*;
use quickcheck::{Arbitrary, Gen};
use rand::distributions::uniform::*;
use rand::distributions::Distribution;
use rand::Rng;
use statrs::distribution::{Bernoulli, Categorical};
use crate::data::HashMap;

impl Arbitrary for EVal {
    fn arbitrary(g: &mut Gen) -> Self {
        // match g.choose(&[0, 1, 2, 3]) {
        match g.choose(&[0]) {
            Some(0) => EVal::EBdd(BddPtr::from_bool(bool::arbitrary(g))),
            // Some(1) => EVal::EFloat(f64::arbitrary(g)),
            // Some(2) => EVal::EInteger(usize::arbitrary(g)),
            // Some(3) => EVal::EProd(Vec::<EVal>::arbitrary(g)), // don't worry about these yet
            _ => panic!("only bools / floats / integers / prods generated"),
        }
    }
}
impl Arbitrary for SVal {
    fn arbitrary(g: &mut Gen) -> Self {
        match g.choose(&[0, 1, 2]) {
            // match g.choose(&[0, 1, 2, 3]) {
            Some(0) => SVal::SBool(bool::arbitrary(g)),
            Some(1) => {
                let f = f64::from(*g.choose(&Vec::from_iter((-5_i32..5).step_by(1))).unwrap());
                SVal::SFloat(f)
            }
            // Some(2) => SVal::SInt(u64::arbitrary(g)),
            // Some(3) => SVal::SProd(Vec::<SVal>::arbitrary(g)),
            Some(2) => {
                // no need to go crazy, just do a small range of these
                let p = g.choose(&[
                    0.0,
                    1.0 / 6.0,
                    2.0 / 6.0,
                    3.0 / 6.0,
                    4.0 / 6.0,
                    5.0 / 6.0,
                    1.0,
                ]);
                SVal::SDist(Dist::Bern(*p.unwrap()))
            }
            _ => panic!("only bools / floats / integers / prods generated"),
        }
    }
}
#[derive(Clone, Debug)]
struct VarStore(Vec<usize>); // generated variable names

#[repr(usize)]
#[derive(Clone, Copy, Debug)]
enum Restriction {
    No = 0,
    Bool = 1,
    Prob = 2,
    Float = 3,
    Int = 4,
}
fn all_restrictions() -> Vec<Restriction> {
    unsafe {
        (Restriction::No as usize..Restriction::Int as usize)
            .map(|x| std::mem::transmute(x))
            .collect::<Vec<Restriction>>()
    }
}
fn condition(g: &mut Gen, r: Restriction, cs: &[Restriction]) -> Restriction {
    use Restriction::*;
    match r {
        No => *g.choose(cs).unwrap(),
        _ => r,
    }
}
fn arb_discrete<T: Copy>(g: &mut Gen, ps: &[(T, usize)]) -> T {
    assert!(ps.len() > 1);
    let key: HashMap<usize, T> = ps.iter().enumerate().map(|(i, (t, w))| (i, *t)).collect();
    let cs = ps
        .iter()
        .enumerate()
        .flat_map(|(i, (t, w))| itertools::repeat_n(i, *w))
        .collect_vec();

    let choice = *g.choose(&cs).unwrap();
    key[&choice]
}

fn arb_flip<T: Copy>(g: &mut Gen, p: Rational32) -> bool {
    let denom = Rational32::from_integer(*p.denom());
    let np = Rational32::one() - p;
    let ratiop = (denom * p).to_integer().try_into().unwrap();
    let rationp = (denom * np).to_integer().try_into().unwrap();
    return arb_discrete(g, &[(true, ratiop), (false, rationp)]);
}

#[derive(Clone, Copy, Debug)]
enum EAPath {
    Var,
    Val,
    And,
    Or,
    Neg,
    Plu,
    Min,
    Mul,
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
}
fn choose_var<T: Debug + PartialEq + Clone, X: Debug + PartialEq + Clone>(
    g: &mut Gen,
    vars: &VarStore,
) -> Anf<Inferable, X>
where
    AVarExt<X>: ξ<Inferable, Ext = Option<T>>,
    AValExt<X>: ξ<Inferable, Ext = ()>,
    ADistExt<X>: ξ<Inferable, Ext = ()>,
{
    let v = g.choose(&vars.0).unwrap();
    Anf::AVar(None, format!("v{}", v))
}
fn partial_arbitrary_eanf(g: &mut Gen, vars: &VarStore, r: Restriction) -> Anf<Inferable, EVal> {
    use EAPath::*;
    use Restriction::*;
    match r {
        No => {
            let r = g.choose(&[Bool, Prob, Float]).unwrap();
            partial_arbitrary_eanf(g, vars, *r)
        }
        Bool => {
            let path = arb_discrete(
                g,
                &[(Var, 4), (Val, 16), (And, 1), (Or, 1), (Neg, 1), (Eq, 1)],
            );
            match path {
                Var => {
                    if vars.0.len() > 0 {
                        choose_var(g, vars)
                    } else {
                        partial_arbitrary_eanf(g, vars, r)
                    }
                }
                Val => Anf::AVal((), EVal::EBdd(BddPtr::from_bool(bool::arbitrary(g)))),
                And => {
                    let l = partial_arbitrary_eanf(g, vars, Bool);
                    let r = partial_arbitrary_eanf(g, vars, Bool);
                    Anf::And(Box::new(l), Box::new(r))
                }
                Or => {
                    let l = partial_arbitrary_eanf(g, vars, Bool);
                    let r = partial_arbitrary_eanf(g, vars, Bool);
                    Anf::Or(Box::new(l), Box::new(r))
                }
                Eq => {
                    let l = partial_arbitrary_eanf(g, vars, Bool);
                    let r = partial_arbitrary_eanf(g, vars, Bool);
                    Anf::EQ(Box::new(l), Box::new(r))
                }
                Neg => {
                    let l = partial_arbitrary_eanf(g, vars, Bool);
                    Anf::Neg(Box::new(l))
                }
                p => panic!(
                    "invalid exact generation! path: {:?}, restriction: {:?}",
                    p, r
                ),
            }
        }
        Float | Int | Prob => {
            let path = arb_discrete(
                g,
                &[
                    (Var, 4),
                    (Val, 16),
                    (Plu, 1),
                    (Min, 1),
                    (Mul, 1),
                    (Gt, 1),
                    (Lt, 1),
                    (Gte, 1),
                    (Lte, 1),
                    (Eq, 1),
                ],
            );
            match (path, r) {
                (Var, _) => {
                    if vars.0.len() > 0 {
                        choose_var(g, vars)
                    } else {
                        partial_arbitrary_eanf(g, vars, r)
                    }
                }
                (Val, Float) => Anf::AVal((), EVal::EFloat(f64::arbitrary(g))),
                (Val, Prob) => Anf::AVal((), EVal::EFloat(f64::arbitrary(g) % 1.0)),
                (Val, Int) => Anf::AVal((), EVal::EInteger(usize::arbitrary(g))),

                (Plu, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_eanf(g, vars, r);
                    let rhs = partial_arbitrary_eanf(g, vars, r);
                    Anf::Plus(Box::new(lhs), Box::new(rhs))
                }
                (Min, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_eanf(g, vars, r);
                    let rhs = partial_arbitrary_eanf(g, vars, r);
                    Anf::Minus(Box::new(lhs), Box::new(rhs))
                }
                (Mul, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_eanf(g, vars, r);
                    let rhs = partial_arbitrary_eanf(g, vars, r);
                    Anf::Mult(Box::new(lhs), Box::new(rhs))
                }
                (Gt, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_eanf(g, vars, r);
                    let rhs = partial_arbitrary_eanf(g, vars, r);
                    Anf::GT(Box::new(lhs), Box::new(rhs))
                }
                (Lt, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_eanf(g, vars, r);
                    let rhs = partial_arbitrary_eanf(g, vars, r);
                    Anf::LT(Box::new(lhs), Box::new(rhs))
                }
                (Gte, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_eanf(g, vars, r);
                    let rhs = partial_arbitrary_eanf(g, vars, r);
                    Anf::GTE(Box::new(lhs), Box::new(rhs))
                }
                (Lte, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_eanf(g, vars, r);
                    let rhs = partial_arbitrary_eanf(g, vars, r);
                    Anf::LTE(Box::new(lhs), Box::new(rhs))
                }

                (Eq, _) => {
                    let r = condition(g, r, &[Float, Int, Prob, Bool]);
                    let lhs = partial_arbitrary_eanf(g, vars, r);
                    let rhs = partial_arbitrary_eanf(g, vars, r);
                    Anf::EQ(Box::new(lhs), Box::new(rhs))
                }

                (p, r) => panic!(
                    "invalid exact generation! path: {:?}, restriction: {:?}",
                    p, r
                ),
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum SAPath {
    Var,
    Val,
    And,
    Or,
    Neg,
    Plu,
    Min,
    Mul,
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    Bern,
}
fn partial_arbitrary_sanf(g: &mut Gen, vars: &VarStore, r: Restriction) -> Anf<Inferable, SVal> {
    use EAPath::*;
    use Restriction::*;
    match r {
        No => {
            if bool::arbitrary(g) {
                let p = f64::arbitrary(g) % 1.0;
                Anf::AnfBernoulli((), Box::new(Anf::AVal((), SVal::SFloat(p))))
            } else {
                let r = g.choose(&[Bool, Prob, Float]).unwrap();
                partial_arbitrary_sanf(g, vars, *r)
            }
        }
        Bool => {
            let path = arb_discrete(
                g,
                &[(Var, 4), (Val, 16), (And, 1), (Or, 1), (Neg, 1), (Eq, 1)],
            );
            match path {
                Var => {
                    if vars.0.len() > 0 {
                        choose_var(g, vars)
                    } else {
                        partial_arbitrary_sanf(g, vars, r)
                    }
                }
                Val => Anf::AVal((), SVal::SBool(bool::arbitrary(g))),
                And => {
                    let l = partial_arbitrary_sanf(g, vars, Bool);
                    let r = partial_arbitrary_sanf(g, vars, Bool);
                    Anf::And(Box::new(l), Box::new(r))
                }
                Or => {
                    let l = partial_arbitrary_sanf(g, vars, Bool);
                    let r = partial_arbitrary_sanf(g, vars, Bool);
                    Anf::Or(Box::new(l), Box::new(r))
                }
                Neg => {
                    let l = partial_arbitrary_sanf(g, vars, Bool);
                    Anf::Neg(Box::new(l))
                }
                Eq => {
                    let l = partial_arbitrary_sanf(g, vars, Bool);
                    let r = partial_arbitrary_sanf(g, vars, Bool);
                    Anf::EQ(Box::new(l), Box::new(r))
                }
                p => panic!(
                    "invalid sample generation! path: {:?}, restriction: {:?}",
                    p, r
                ),
            }
        }
        Float | Int | Prob => {
            let path = arb_discrete(
                g,
                &[
                    (Var, 4),
                    (Val, 16),
                    (Plu, 1),
                    (Min, 1),
                    (Mul, 1),
                    (Gt, 1),
                    (Lt, 1),
                    (Gte, 1),
                    (Lte, 1),
                    (Eq, 1),
                ],
            );
            match (path, r) {
                (Var, _) => {
                    if vars.0.len() > 0 {
                        choose_var(g, vars)
                    } else {
                        partial_arbitrary_sanf(g, vars, r)
                    }
                }
                (Val, Float) => Anf::AVal((), SVal::SFloat(f64::arbitrary(g))),
                (Val, Prob) => Anf::AVal((), SVal::SFloat(f64::arbitrary(g) % 1.0)),
                (Val, Int) => Anf::AVal((), SVal::SInt(u64::arbitrary(g))),

                (Plu, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_sanf(g, vars, r);
                    let rhs = partial_arbitrary_sanf(g, vars, r);
                    Anf::Plus(Box::new(lhs), Box::new(rhs))
                }
                (Min, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_sanf(g, vars, r);
                    let rhs = partial_arbitrary_sanf(g, vars, r);
                    Anf::Minus(Box::new(lhs), Box::new(rhs))
                }
                (Mul, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_sanf(g, vars, r);
                    let rhs = partial_arbitrary_sanf(g, vars, r);
                    Anf::Mult(Box::new(lhs), Box::new(rhs))
                }
                (Gt, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_sanf(g, vars, r);
                    let rhs = partial_arbitrary_sanf(g, vars, r);
                    Anf::GT(Box::new(lhs), Box::new(rhs))
                }
                (Lt, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_sanf(g, vars, r);
                    let rhs = partial_arbitrary_sanf(g, vars, r);
                    Anf::LT(Box::new(lhs), Box::new(rhs))
                }
                (Gte, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_sanf(g, vars, r);
                    let rhs = partial_arbitrary_sanf(g, vars, r);
                    Anf::GTE(Box::new(lhs), Box::new(rhs))
                }
                (Lte, _) => {
                    let r = condition(g, r, &[Float, Int, Prob]);
                    let lhs = partial_arbitrary_sanf(g, vars, r);
                    let rhs = partial_arbitrary_sanf(g, vars, r);
                    Anf::LTE(Box::new(lhs), Box::new(rhs))
                }

                (Eq, _) => {
                    let r = condition(g, r, &[Float, Int, Prob, Bool]);
                    let lhs = partial_arbitrary_sanf(g, vars, r);
                    let rhs = partial_arbitrary_sanf(g, vars, r);
                    Anf::EQ(Box::new(lhs), Box::new(rhs))
                }

                (p, r) => panic!(
                    "invalid sample generation! path: {:?}, restriction: {:?}",
                    p, r
                ),
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum EPath {
    Anf,
    Let,
    If,
    Flip,
    Obs,
    Samp,
}
fn partial_arbitrary_eexpr(g: &mut Gen, vars: &VarStore, r: Restriction) -> EExpr<Inferable> {
    use EExpr::*;
    use EPath::*;
    use Restriction::*;
    let path = arb_discrete(
        g,
        &[(Anf, 4), (Let, 2), (If, 2), (Flip, 4), (Obs, 2), (Samp, 4)],
    );
    match path {
        Anf => EAnf((), Box::new(partial_arbitrary_eanf(g, vars, r))),
        Let => {
            let bind = partial_arbitrary_eexpr(g, vars, r);
            let v = vars.0.len();
            let lbl = format!("v{}", v);
            let mut vars = vars.0.clone();
            vars.push(v);
            let over = partial_arbitrary_eexpr(g, &VarStore(vars), r);
            ELetIn(None, lbl, Box::new(bind), Box::new(over))
        }
        If => {
            let p = partial_arbitrary_sanf(g, vars, Bool);
            let v = vars.0.len();
            let lbl = format!("v{}", v);
            // vars.0.push(v);
            let sp = partial_arbitrary_sanf(g, vars, Bool);
            let r = g.choose(&[Bool, Float, Prob]).unwrap();

            let t = partial_arbitrary_eexpr(g, vars, *r);
            let f = partial_arbitrary_eexpr(g, vars, *r);
            ELetIn(
                None,
                lbl.clone(),
                Box::new(ESample((), Box::new(SExpr::SAnf((), Box::new(sp))))),
                Box::new(EIte(
                    None,
                    Box::new(crate::Anf::AVar(None, lbl)),
                    Box::new(t),
                    Box::new(f),
                )),
            )
        }
        Flip => EFlip((), Box::new(partial_arbitrary_eanf(g, vars, Prob))),
        Obs => {
            let obs = Box::new(partial_arbitrary_eanf(g, vars, Bool));
            EObserve((), obs, Box::new(partial_arbitrary_eexpr(g, vars, r)))
        }
        Samp => ESample((), Box::new(partial_arbitrary_sexpr(g, vars, r))),
    }
}
#[derive(Clone, Copy, Debug)]
enum SPath {
    Anf,
    Let,
    Sim,
    If,
    Exact,
}

fn partial_arbitrary_sexpr(g: &mut Gen, vars: &VarStore, r: Restriction) -> SExpr<Inferable> {
    use Restriction::*;
    use SExpr::*;
    use SPath::*;
    let path = arb_discrete(g, &[(Anf, 4), (Let, 2), (If, 2), (Sim, 4), (Exact, 4)]);
    match path {
        Anf => SAnf((), Box::new(partial_arbitrary_sanf(g, vars, No))),
        Let => {
            let bind = partial_arbitrary_sexpr(g, vars, r);
            let v = vars.0.len();
            let lbl = format!("v{}", v);
            let mut vars = vars.0.clone();
            vars.push(v);
            let over = partial_arbitrary_sexpr(g, &VarStore(vars), r);
            SLetIn(None, lbl, Box::new(bind), Box::new(over))
        }
        If => {
            let p = partial_arbitrary_sanf(g, vars, Bool);
            let r = g.choose(&[Bool, Float, Prob]).unwrap();

            let t = partial_arbitrary_sexpr(g, vars, *r);
            let f = partial_arbitrary_sexpr(g, vars, *r);
            SIte(None, Box::new(p), Box::new(t), Box::new(f))
        }
        Sim => {
            let v = vars.0.len();
            let lbl = format!("v{}", v);
            let mut vars = vars.0.clone();
            vars.push(v);
            let p = f64::arbitrary(g) % 1.0;
            let bind = SExpr::SAnf(
                (),
                Box::new(crate::Anf::AnfBernoulli(
                    (),
                    Box::new(crate::Anf::AVal((), SVal::SFloat(p))),
                )),
            );
            let over = partial_arbitrary_sexpr(g, &VarStore(vars), r);
            SLetIn(None, lbl, Box::new(bind), Box::new(over))
        }
        Exact => SExact((), Box::new(partial_arbitrary_eexpr(g, vars, r))),
    }
}

impl Arbitrary for Program<Inferable> {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut vs = VarStore(vec![]);
        if bool::arbitrary(g) {
            Program::EBody(partial_arbitrary_eexpr(g, &mut vs, Restriction::Bool))
        } else {
            Program::SBody(partial_arbitrary_sexpr(g, &mut vs, Restriction::Bool))
        }
    }
}

pub fn run_property(precision: Option<f64>, n: Option<usize>, p: &Program<Inferable>) -> bool {
    let precision = precision.unwrap_or(0.01);
    let n = n.unwrap_or(10000);
    let exact = exact_inferable(p).0;
    let (approx, _) = importance_weighting_inferable(
        n,
        p,
        &Options {
            opt: false,
            debug: false,
            // seed: Some(9),
            ..Default::default()
        },
    );
    println!("exact:  {:?}", exact);
    println!("approx: {:?}", approx);

    assert_eq!(
        exact.len(),
        approx.len(),
        "[check_inv][mismatch shape] compiled exact queries {}, but approx returned results {}",
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
                "[check_inv][#{i}][err]((exact: {ext}) - (approx: {apx})).abs < {precision}"
            );
        });
    true
}

#[quickcheck]
fn prop_erasure(p: Program<Inferable>) -> bool {
    println!("running...");
    let erased = p.strip_samples();
    println!("program: {:?}", p);
    println!(" erased: {:?}", erased);

    run_property(Some(0.01), Some(100), &p)
}

#[test]
fn test_prop_ex1() {
    let p = Program::SBody(SExpr::SAnf(
        (),
        Box::new(Anf::AnfBernoulli(
            (),
            Box::new(Anf::AVal((), SVal::SFloat(-0.0))),
        )),
    ));
    run_property(Some(0.01), Some(100), &p);
}

#[test]
fn test_prop_ex2() {
    use Anf::*;
    use BddPtr::*;
    use EExpr::*;
    use EVal::*;
    use Option::*;
    use SExpr::*;
    use SVal::*;
    let p = Program::EBody(EIte(
        None,
        Box::new(AVal((), EBdd(PtrTrue))),
        Box::new(ELetIn(
            None,
            "v0".to_string(),
            Box::new(ELetIn(
                None,
                "v1".to_string(),
                Box::new(EFlip((), Box::new(Anf::AVal((), EFloat(0.0))))),
                Box::new(EAnf((), Box::new(AVal((), EBdd(PtrTrue))))),
            )),
            Box::new(ELetIn(
                None,
                "v2".to_string(),
                Box::new(ELetIn(
                    None,
                    "v3".to_string(),
                    Box::new(EAnf((), Box::new(AVal((), EFloat(0.0))))),
                    // TODO this is a boolean, we need to check gamma, but we don't do type checking right now
                    Box::new(EFlip((), Box::new(AVar(None, "v0".to_string())))),
                )),
                Box::new(EFlip((), Box::new(AVal((), EFloat(-0.0))))),
            )),
        )),
        Box::new(EFlip((), Box::new(AVal((), EFloat(-0.0))))),
    ));
    run_property(Some(0.01), Some(100), &p);
}
