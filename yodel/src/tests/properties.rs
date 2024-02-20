use crate::compile::*;
use crate::grammar::*;
use crate::inference::*;
use crate::tests::checks::*;
use crate::typeinf::grammar::*;
use crate::*;

use quickcheck::{Arbitrary, Gen};
use rand::distributions::uniform::*;
use rand::distributions::Distribution;
use rand::Rng;
use statrs::distribution::{Bernoulli, Categorical};

impl Arbitrary for EVal {
    fn arbitrary(g: &mut Gen) -> Self {
        match g.choose(&[0, 1, 2, 3]) {
            Some(0) => EVal::EBdd(BddPtr::from_bool(bool::arbitrary(g))),
            Some(1) => EVal::EFloat(f64::arbitrary(g)),
            Some(2) => EVal::EInteger(usize::arbitrary(g)),
            Some(3) => EVal::EProd(Vec::<EVal>::arbitrary(g)),
            _ => panic!("only bools / floats / integers / prods generated"),
        }
    }
}
impl Arbitrary for SVal {
    fn arbitrary(g: &mut Gen) -> Self {
        match g.choose(&[0, 1, 2, 3]) {
            Some(0) => SVal::SBool(bool::arbitrary(g)),
            Some(1) => SVal::SFloat(f64::arbitrary(g)),
            Some(2) => SVal::SInt(u64::arbitrary(g)),
            Some(3) => SVal::SProd(Vec::<SVal>::arbitrary(g)),
            Some(4) => {
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

impl Arbitrary for VarStore {
    fn arbitrary(g: &mut Gen) -> Self {
        VarStore(Vec::from_iter(0..=usize::arbitrary(g) % 20))
    }
}
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
fn partial_arbitrary_eanf(g: &mut Gen, vars: &VarStore, r: Restriction) -> Anf<Inferable, EVal> {
    use Restriction::*;
    match (g.choose(&Vec::from_iter(0..=13)).unwrap(), r) {
        (0, _) => {
            let v = g.choose(&vars.0).unwrap();
            Anf::AVar(None, format!("v{}", v))
        }
        (1, No) => Anf::AVal((), EVal::arbitrary(g)),
        (1, Bool) => Anf::AVal((), EVal::EBdd(BddPtr::from_bool(bool::arbitrary(g)))),
        (1, Float) => Anf::AVal((), EVal::EFloat(f64::arbitrary(g))),
        (1, Prob) => Anf::AVal((), EVal::EFloat(f64::arbitrary(g) % 1.0)),
        (1, Int) => Anf::AVal((), EVal::EInteger(usize::arbitrary(g))),
        (2, No) | (2, Bool) => {
            let l = partial_arbitrary_eanf(g, vars, Bool);
            let r = partial_arbitrary_eanf(g, vars, Bool);
            Anf::And(Box::new(l), Box::new(r))
        }
        (3, No) | (3, Bool) => {
            let l = partial_arbitrary_eanf(g, vars, Bool);
            let r = partial_arbitrary_eanf(g, vars, Bool);
            Anf::Or(Box::new(l), Box::new(r))
        }
        (4, No) | (4, Bool) => {
            let l = partial_arbitrary_eanf(g, vars, Bool);
            Anf::Neg(Box::new(l))
        }
        (5, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::Plus(Box::new(lhs), Box::new(rhs))
        }
        (6, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::Minus(Box::new(lhs), Box::new(rhs))
        }
        (7, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::Mult(Box::new(lhs), Box::new(rhs))
        }
        (8, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::Div(Box::new(lhs), Box::new(rhs))
        }
        (9, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::GT(Box::new(lhs), Box::new(rhs))
        }
        (10, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::LT(Box::new(lhs), Box::new(rhs))
        }
        (11, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::GTE(Box::new(lhs), Box::new(rhs))
        }
        (12, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::LTE(Box::new(lhs), Box::new(rhs))
        }
        (13, _) => {
            let r = condition(g, r, &[Float, Int, Prob, Bool]);
            let lhs = partial_arbitrary_eanf(g, vars, r);
            let rhs = partial_arbitrary_eanf(g, vars, r);
            Anf::EQ(Box::new(lhs), Box::new(rhs))
        }
        // Prods, Vecs, Projections are skipped
        // (14, _) => {
        //     // TODO: for now, if we generate an unrestricted vector, it will be a bool vector
        //     // TODO: generate var-referencing vectors
        //     let sz = 1 + g.choose(usize).unwrap() % 10;
        //     let vs = match r {
        //         No | Bool => (0..sz).map(|i| SVal::SBool(bool::arbitrary(g))).collect_vec(),
        //         Int => (0..sz).map(|i| SVal::SInt(u64::arbitrary(g))).collect_vec(),
        //         Float => (0..sz).map(|i| SVal::SFloat(f64::arbitrary(g))).collect_vec(),
        //     };
        //     AnfVec(vs)
        // },
        // // skip prod for now
        // (15, _) => {
        // },

        // only for SVars
        // (14, _) => {
        //   let p = f64::arbitrary(g) % 1.0;
        //   Anf::AnfBernoulli((), Box::new(Anf::AVal((), EVal::EFloat(p))))
        // }
        _ => panic!("invalid generation!"),
    }
}
fn partial_arbitrary_sanf(g: &mut Gen, vars: &VarStore, r: Restriction) -> Anf<Inferable, SVal> {
    use Restriction::*;
    match (g.choose(&Vec::from_iter(0..=14)).unwrap(), r) {
        (0, _) => {
            let v = g.choose(&vars.0).unwrap();
            Anf::AVar(None, format!("v{}", v))
        }
        (1, No) => Anf::AVal((), SVal::arbitrary(g)),
        (1, Bool) => Anf::AVal((), SVal::SBool(bool::arbitrary(g))),
        (1, Float) => Anf::AVal((), SVal::SFloat(f64::arbitrary(g))),
        (1, Int) => Anf::AVal((), SVal::SInt(u64::arbitrary(g))),
        (2, No) | (2, Bool) => {
            let l = partial_arbitrary_sanf(g, vars, Bool);
            let r = partial_arbitrary_sanf(g, vars, Bool);
            Anf::And(Box::new(l), Box::new(r))
        }
        (3, No) | (3, Bool) => {
            let l = partial_arbitrary_sanf(g, vars, Bool);
            let r = partial_arbitrary_sanf(g, vars, Bool);
            Anf::Or(Box::new(l), Box::new(r))
        }
        (4, No) | (4, Bool) => {
            let l = partial_arbitrary_sanf(g, vars, Bool);
            Anf::Neg(Box::new(l))
        }
        (5, _) => {
            let r = condition(g, r, &[Float, Int, Prob]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::Plus(Box::new(lhs), Box::new(rhs))
        }
        (6, _) => {
            let r = condition(g, r, &[Float, Int]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::Minus(Box::new(lhs), Box::new(rhs))
        }
        (7, _) => {
            let r = condition(g, r, &[Float, Int]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::Mult(Box::new(lhs), Box::new(rhs))
        }
        (8, _) => {
            let r = condition(g, r, &[Float, Int]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::Div(Box::new(lhs), Box::new(rhs))
        }
        (9, _) => {
            let r = condition(g, r, &[Float, Int]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::GT(Box::new(lhs), Box::new(rhs))
        }
        (10, _) => {
            let r = condition(g, r, &[Float, Int]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::LT(Box::new(lhs), Box::new(rhs))
        }
        (11, _) => {
            let r = condition(g, r, &[Float, Int]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::GTE(Box::new(lhs), Box::new(rhs))
        }
        (12, _) => {
            let r = condition(g, r, &[Float, Int]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::LTE(Box::new(lhs), Box::new(rhs))
        }
        (13, _) => {
            let r = condition(g, r, &[Float, Int, Bool]);
            let lhs = partial_arbitrary_sanf(g, vars, r);
            let rhs = partial_arbitrary_sanf(g, vars, r);
            Anf::EQ(Box::new(lhs), Box::new(rhs))
        }
        // Prods, Vecs, Projections are skipped
        (14, _) => {
            let p = f64::arbitrary(g) % 1.0;
            Anf::AnfBernoulli((), Box::new(Anf::AVal((), SVal::SFloat(p))))
        }
        _ => panic!("invalid generation!"),
    }
}

fn partial_arbitrary_eexpr(g: &mut Gen, vars: &mut VarStore) -> EExpr<Inferable> {
    use EExpr::*;
    use Restriction::*;
    match g.choose(&Vec::from_iter(0..=5)).unwrap() {
        0 => EAnf((), Box::new(partial_arbitrary_eanf(g, vars, No))),
        1 => {
            let v = vars.0.len();
            let lbl = format!("v{}", v);
            vars.0.push(v);
            let bind = partial_arbitrary_eexpr(g, vars);
            let over = partial_arbitrary_eexpr(g, vars);
            ELetIn(None, lbl, Box::new(bind), Box::new(over))
        }
        2 => {
            let p = partial_arbitrary_sanf(g, vars, Bool);
            let v = vars.0.len();
            let lbl = format!("v{}", v);
            vars.0.push(v);

            let sp = partial_arbitrary_sanf(g, vars, Bool);

            let t = partial_arbitrary_eexpr(g, vars);
            let f = partial_arbitrary_eexpr(g, vars);

            ELetIn(
                None,
                lbl.clone(),
                Box::new(ESample((), Box::new(SExpr::SAnf((), Box::new(sp))))),
                Box::new(EIte(
                    None,
                    Box::new(Anf::AVar(None, lbl)),
                    Box::new(t),
                    Box::new(f),
                )),
            )
        }
        3 => EFlip((), Box::new(partial_arbitrary_eanf(g, vars, Prob))),
        4 => {
            let obs = Box::new(partial_arbitrary_eanf(g, vars, Prob));
            EObserve((), obs, Box::new(partial_arbitrary_eexpr(g, vars)))
        }
        5 => ESample((), Box::new(partial_arbitrary_sexpr(g, vars))),
        _ => panic!("invalid generation!"),
    }
}
fn partial_arbitrary_sexpr(g: &mut Gen, vars: &mut VarStore) -> SExpr<Inferable> {
    use Restriction::*;
    use SExpr::*;
    match g.choose(&Vec::from_iter(0..=4)).unwrap() {
        0 => SAnf((), Box::new(partial_arbitrary_sanf(g, vars, No))),
        1 => {
            let v = vars.0.len();
            let lbl = format!("v{}", v);
            vars.0.push(v);
            let bind = partial_arbitrary_sexpr(g, vars);
            let over = partial_arbitrary_sexpr(g, vars);
            SLetIn(None, lbl, Box::new(bind), Box::new(over))
        }
        2 => {
            let p = partial_arbitrary_sanf(g, vars, Bool);
            let t = partial_arbitrary_sexpr(g, vars);
            let f = partial_arbitrary_sexpr(g, vars);
            SIte(None, Box::new(p), Box::new(t), Box::new(f))
        }
        3 => {
            let v = vars.0.len();
            let lbl = format!("v{}", v);
            vars.0.push(v);
            let p = f64::arbitrary(g) % 1.0;
            let bind = SExpr::SAnf(
                (),
                Box::new(Anf::AnfBernoulli(
                    (),
                    Box::new(Anf::AVal((), SVal::SFloat(p))),
                )),
            );
            let over = partial_arbitrary_sexpr(g, vars);
            SLetIn(None, lbl, Box::new(bind), Box::new(over))
        }
        4 => SExact((), Box::new(partial_arbitrary_eexpr(g, vars))),
        _ => panic!("invalid generation!"),
    }
}
impl Arbitrary for Program<Inferable> {
    fn arbitrary(g: &mut Gen) -> Self {
        let mut vs = VarStore(vec![]);
        if bool::arbitrary(g) {
            Program::EBody(partial_arbitrary_eexpr(g, &mut vs))
        } else {
            Program::SBody(partial_arbitrary_sexpr(g, &mut vs))
        }
    }
}
