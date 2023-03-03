use super::Expr::*;
use crate::grammar::*;
use crate::typecheck::grammar::*;
use crate::ExprTyped;
use crate::*;
use itertools::*;
use rsdd::sample::probability::*;
use std::collections::{HashMap, HashSet, VecDeque};
use tracing_test::traced_test;

pub struct GridSpec<'a> {
    size: usize,
    sampled: bool,
    query: ExprTyped,
    probability: &'a dyn Fn(Ix, Parents<bool>) -> Probability,
}

#[derive(Eq, Hash, PartialEq, Clone, Copy, Debug)]
pub struct Ix(usize, usize);

impl Ix {
    pub fn new(i: usize, j: usize) -> Ix {
        Ix(i, j)
    }
    pub fn as_string(&self) -> String {
        format!("{}{}", self.0, self.1)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Parents<X> {
    Zero,
    One(Ix, X),
    Two((Ix, X), (Ix, X)),
}
impl<X> Parents<X> {
    pub fn to_vec(&self) -> Vec<Ix> {
        use Parents::*;
        match self {
            Zero => vec![],
            One(i, _) => vec![*i],
            Two((i, _), (j, _)) => vec![*i, *j],
        }
    }
}

pub struct GridSchema<'a> {
    tril: HashSet<Ix>,
    triu: HashSet<Ix>,
    diagonal: HashSet<Ix>,

    flips: HashMap<Ix, Probability>,
    parents: HashMap<Ix, Parents<()>>,
    size: usize,
    sampled: bool,
    query: ExprTyped,
    probability: &'a dyn Fn(Ix, Parents<bool>) -> Probability,
}
impl<'a> GridSchema<'a> {
    pub fn get_flip(&self, ix: Ix) -> f64 {
        self.flips.get(&ix).unwrap().as_f64()
    }
    pub fn get_parents(&self, ix: Ix) -> Parents<()> {
        *self.parents.get(&ix).unwrap()
    }
    pub fn get_parents_vec(&self, ix: Ix) -> Vec<Ix> {
        self.get_parents(ix).to_vec()
    }
}
fn get_parents(ix: Ix, size: usize) -> Parents<()> {
    use Parents::*;
    let p0 = usize::checked_sub(ix.0, 1).map(|i0| Ix::new(i0, ix.1));
    let p1 = usize::checked_sub(ix.1, 1).map(|i1| Ix::new(ix.0, i1));
    match (p0, p1) {
        (None, None) => Zero,
        (Some(p), None) => One(p, ()),
        (None, Some(p)) => One(p, ()),
        (Some(l), Some(r)) => Two((l, ()), (r, ())),
    }
}

pub fn make_grid_schema(spec: GridSpec) -> GridSchema {
    let mut flips = HashMap::new();
    let mut parents = HashMap::new();
    let mut diagonal = HashSet::new();
    let mut tril = HashSet::new();
    let mut triu = HashSet::new();
    let sampled = spec.sampled;
    let query = spec.query;

    for tuple in iproduct!((0..spec.size), (0..spec.size)) {
        let i = Ix::new(tuple.0, tuple.1);
        flips.insert(i, (spec.probability)(i, Parents::Zero));
        let ps = get_parents(i, spec.size);
        parents.insert(i, ps);
        if (i.0 + i.1) < (spec.size - 1) {
            triu.insert(i);
        } else if (i.0 + i.1) > (spec.size - 1) {
            tril.insert(i);
        } else {
            diagonal.insert(i);
        }
    }

    GridSchema {
        flips,
        parents,
        tril,
        triu,
        diagonal,
        sampled,
        query,
        size: spec.size,
        probability: spec.probability,
    }
}

#[rustfmt::skip]
mod make {
    use super::*;
    pub fn no_parents(schema: &GridSchema, ix: Ix, rest: ExprTyped) -> ExprTyped {
        let pr = (schema.probability)(ix, Parents::Zero).as_f64();
        ELetIn(
            LetInTypes {
                bindee: b!(),
                body: b!(),
            },
            ix.as_string(),
            Box::new(EFlip((), pr)),
            Box::new(rest.clone()),
        )
    }

    pub fn one_parents(schema: &GridSchema, ix: Ix, parent: Ix, rest: ExprTyped) -> ExprTyped {
        let prob = schema.probability;
        let p_t = prob(ix, Parents::One(parent, true)).as_f64();
        let p_f = prob(ix, Parents::One(parent, false)).as_f64();
        let p = parent.as_string();

        ELetIn(
            LetInTypes {bindee: b!(), body: b!(),},
            ix.as_string(),
            Box::new(ite![(var!(@ p)) ? (flip!(@ p_t)) : (flip!(@ p_t))]),
            Box::new(rest),
        )
    }

    pub fn two_parents(schema: &GridSchema, ix: Ix, parents: (Ix, Ix), rest: ExprTyped) -> ExprTyped {
        let prob = schema.probability;
        let (pl, pr) = parents;
        let (pl, pr) = (pl.as_string(), pr.as_string());
        let p_t_t = prob(ix, Parents::Two((parents.0, true), (parents.1, true))).as_f64();
        let p_f_t = prob(ix, Parents::Two((parents.0, false), (parents.1, true))).as_f64();
        let p_t_f = prob(ix, Parents::Two((parents.0, true), (parents.1, false))).as_f64();
        let p_f_f = prob(ix, Parents::Two((parents.0, false), (parents.1, false))).as_f64();
        ELetIn(
            LetInTypes {bindee: b!(), body: b!(),},
            ix.as_string(),
            Box::new(
                ite!((b!((var!(@ pl)) && (var!(@ pr)))) ? (flip!(@ p_t_t)) : (
                ite!((b!((var!(@ pl)) && (not!(@ pr)))) ? (flip!(@ p_t_f)) : (
                ite!((b!((not!(@ pl)) && (var!(@ pr)))) ? (flip!(@ p_f_t)) : (
                                                           flip!(@ p_f_f)))))))
            ),
            Box::new(rest),
        )
    }

    pub fn diag_alias(schema: &GridSchema, ix: Ix, prj: usize, rest: ExprTyped) -> ExprTyped {
        ELetIn(
            LetInTypes {bindee: b!(), body: b!(),},
            ix.as_string(),
            Box::new(EPrj(b!(), prj, Box::new(var!(@ "diag")))),
            Box::new(rest),
        )
    }
    pub fn diag_ixs(schema: &GridSchema) -> Vec<Ix> {
        let mut diag = vec![];
        let mut j = 0;
        for i in schema.size..0 {
            diag.push(Ix::new(j, i));
            j+=1;
        }
        diag
    }
    pub fn product_of(ixs:Vec<Ix>) -> ExprTyped {
        let mut prod = vec![];
        let mut ty = vec![];
        for i in ixs {
            let s = i.as_string();
            prod.push(var!(@ s));
            ty.push(Ty::Bool);
        }
        Expr::EProd(Ty::Prod(ty), prod)
    }
    pub fn all_diag_aliases(schema: &GridSchema, rest: ExprTyped) -> ExprTyped {
        diag_ixs(schema)
            .iter()
            .cloned()
            .enumerate()
            .fold(rest, |prg, (i, ix) | diag_alias(schema, ix, i, prg))
    }
    pub fn sample_diag(prg: ExprTyped, tril: ExprTyped) -> ExprTyped {
        ELetIn(
            LetInTypes {bindee: b!(), body: b!(),},
            "diag".to_string(),
            Box::new(ESample((), Box::new(prg))),
            Box::new(tril),
        )
    }

    pub fn grid(schema: GridSchema) -> ExprTyped {
        use Parents::*;
        let ix = Ix::new(schema.size - 1, schema.size - 1);
        let mut seen = HashSet::from([ix]);
        let mut prg = make::no_parents(&schema, ix, schema.query.clone());
        let mut next_tril = VecDeque::from(schema.get_parents_vec(ix));
        let mut next_diag = VecDeque::new();
        let mut next_triu = VecDeque::new();
        while let Some(ix) = next_tril.pop_front() {
            if seen.contains(&ix) {
                continue;
            } else if !schema.tril.contains(&ix) {
                next_diag.push_back(ix);
            } else {
                let parents = schema.parents.get(&ix).unwrap();
                prg = match parents {
                    Zero => todo!(),
                    One(p, _) => make::one_parents(&schema, ix, *p, prg),
                    Two((l, _), (r, _)) => make::two_parents(&schema, ix, (*l, *r), prg),
                };
                seen.insert(ix);
                let mut nxt_parents = parents.to_vec().into();
                next_tril.append(&mut nxt_parents);
            }
        }
        let mut tril_prg : Option<ExprTyped> = None;
        if schema.sampled {
            prg = all_diag_aliases(&schema, prg);
            tril_prg = Some(prg);
            let ixs = make::diag_ixs(&schema);
            prg = make::product_of(ixs);
        }
        while let Some(ix) = next_diag.pop_front() {
            if seen.contains(&ix) {
                continue;
            } else if !schema.diagonal.contains(&ix) {
                next_triu.push_back(ix);
            } else {
                let parents = schema.parents.get(&ix).unwrap();
                prg = match parents {
                    Zero => todo!(),
                    One(p, _) => make::one_parents(&schema, ix, *p, prg),
                    Two((l, _), (r, _)) => make::two_parents(&schema, ix, (*l, *r), prg),
                };
                seen.insert(ix);
                let mut nxt_parents = parents.to_vec().into();
                next_diag.append(&mut nxt_parents);
            }
        }
        if schema.sampled {
            prg = make::sample_diag(prg, tril_prg.unwrap());
        }
        while let Some(ix) = next_triu.pop_front() {
            if seen.contains(&ix) {
                continue;
            } else {
                let parents = schema.parents.get(&ix).unwrap();
                prg = match parents {
                    Zero => todo!(),
                    One(p, _) => make::one_parents(&schema, ix, *p, prg),
                    Two((l, _), (r, _)) => make::two_parents(&schema, ix, (*l, *r), prg),
                };
                seen.insert(ix);
                let mut nxt_parents = parents.to_vec().into();
                next_triu.append(&mut nxt_parents);
            }
        }
        prg
    }
}

#[test]
#[traced_test]
fn test_grid_schema() {
    let x = make_grid_schema(GridSpec {
        size: 2,
        sampled: true,
        query: b!("x"),
        probability: &|_, _| Probability::new(1.0),
    });
}

// #[test]
// // #[traced_test]
// fn grid2x2() {
//     let mk = |ret: ExprTyped| {
//         Program::Body(lets![
//             "00" ; B!() ;= flip!(1/2);
//             "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
//             "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
//             "11" ; B!() ;=
//                 ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
//                 ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
//                 ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
//                                                           flip!(1/11) ))))));
//             ...? ret ; B!()
//         ])
//     };
//     check_exact1("grid2x2/3/00", 1.0 / 2.0, &mk(b!("00")));
//     check_exact1("grid2x2/3/01", 0.291666667, &mk(b!("01")));
//     check_exact1("grid2x2/3/10", 0.183333333, &mk(b!("10")));
//     check_exact1("grid2x2/3/11", 0.102927589, &mk(b!("11")));
// }

// #[test]
// // #[traced_test]
// fn grid2x2_sampled() {
//     let mk = |ret: ExprTyped| {
//         Program::Body(lets![
//             "00" ; B!() ;= flip!(1/2);
//             "01_10" ; b!(B, B) ;= sample!(
//                 lets![
//                     "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
//                     "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
//                     ...? b!("01", "10") ; b!(B, B)
//                 ]);
//             "01" ; B!() ;= fst!("01_10");
//             "10" ; B!() ;= snd!("01_10");
// "11" ; B!() ;=
//     ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
//     ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
//     ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
//                                               flip!(1/11) ))))));
//             ...? ret ; B!()
//         ])
//     };
//     // check_approx1("grid2x2/approx_diag/00", 1.0 / 2.0, &mk(b!("00")), 10000);
//     // check_approx1("grid2x2/approx_diag/01", 0.291666667, &mk(b!("01")), 10000);
//     // check_approx1("grid2x2/approx_diag/10", 0.183333333, &mk(b!("10")), 10000);
//     // check_approx1("grid2x2/approx_diag/11", 0.102927589, &mk(b!("11")), 10000);

//     check_approx(
//         "grid2x2/approx_diag/00,01,10,11",
//         vec![1.0 / 2.0, 0.291666667, 0.183333333, 0.102927589],
//         &mk(b!("00", "01", "10", "11")),
//         20000,
//     );
// }

// /// a directed 3x3 grid test where we place samples according to various policies
// ///   (0,0) -> (0,1) -> (0,2)
// ///     v        v        v
// ///   (1,0) -> (1,1) -> (1,2)
// ///     v        v        v
// ///   (2,0) -> (2,1) -> (2,2)
// #[test]
// // #[traced_test]
// fn grid3x3_sampled_diag() {
//     let mk = |ret: ExprTyped| {
//         Program::Body(lets![
//             "00" ; B!() ;= flip!(1/2);
//             "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
//             "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );

//             "20_11_02" ; b!(B, B) ;= sample!(
//                 lets![
//                   "20" ; B!() ;= ite!( ( not!("10") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
//                   "11" ; B!() ;=
//                       ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
//                       ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
//                       ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
//                                                                 flip!(1/11) ))))));
//                   "02" ; B!() ;= ite!( ( b!(@anf "01")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
//                           ...? b!("20", "11", "02") ; b!(B, B, B)
//                 ]);
//             "20" ; B!() ;= fst!("20_11_02");
//             "11" ; B!() ;= snd!("20_11_02");
//             "02" ; B!() ;= thd!("20_11_02");

//             "21" ; B!() ;=
//                 ite!(( b!((  b!(@anf "20")) && (  b!(@anf "11"))) ) ? ( flip!(2/7) ) : (
//                 ite!(( b!((  b!(@anf "20")) && (not!("11"))) ) ? ( flip!(2/8) ) : (
//                 ite!(( b!((  not!("20")) && (  b!(@anf "11"))) ) ? ( flip!(2/9) ) : (
//                                                           flip!(2/11) ))))));

//             "12" ; B!() ;=
//                 ite!(( b!((  b!(@anf "11")) && (  b!(@anf "02"))) ) ? ( flip!(6/7) ) : (
//                 ite!(( b!((  b!(@anf "11")) && (not!("02"))) ) ? ( flip!(6/8) ) : (
//                 ite!(( b!((  not!("11")) && (  b!(@anf "02"))) ) ? ( flip!(6/9) ) : (
//                                                           flip!(6/11) ))))));

//             "22" ; B!() ;=
//                 ite!(( b!((  b!(@anf "21")) && (  b!(@anf "12"))) ) ? ( flip!(3/7) ) : (
//                 ite!(( b!((  b!(@anf "21")) && (not!("12"))) ) ? ( flip!(3/8) ) : (
//                 ite!(( b!((  not!("21")) && (  b!(@anf "12"))) ) ? ( flip!(8/9) ) : (
//                                                           flip!(9/11) ))))));
//             ...? ret ; B!()
//         ])
//     };
//     // check_approx1("grid3x3/approx/00", 0.500000000, &mk(b!("00")), 10000);
//     // check_approx1("grid3x3/approx/01", 0.291666667, &mk(b!("01")), 10000);
//     // check_approx1("grid3x3/approx/10", 0.183333333, &mk(b!("10")), 10000);
//     // check_approx1("grid3x3/approx/02", 0.274305556, &mk(b!("02")), 10000);
//     // check_approx1("grid3x3/approx/20", 0.193888889, &mk(b!("20")), 10000);
//     // check_approx1("grid3x3/approx/11", 0.102927589, &mk(b!("11")), 10000);
//     // check_approx1("grid3x3/approx/12", 0.599355085, &mk(b!("12")), 10000);
//     // check_approx1("grid3x3/approx/21", 0.199103758, &mk(b!("21")), 10000);
//     // check_approx1("grid3x3/approx/22", 0.770263904, &mk(b!("22")), 10000);
//     check_approx(
//         "grid3x3/approx/[00,01,10,02,20,11,12,21,22]",
//         vec![
//             0.500000000,
//             0.291666667,
//             0.183333333,
//             0.274305556,
//             0.193888889,
//             0.102927589,
//             0.599355085,
//             0.199103758,
//             0.770263904,
//         ],
//         &mk(b!("00", "01", "10", "02", "20", "11", "12", "21", "22")),
//         20000,
//     );
// }

// /// a directed 3x3 grid test where we place samples according to various policies
// ///   (0,0) -> (0,1) -> (0,2)
// ///     v        v        v
// ///   (1,0) -> (1,1) -> (1,2)
// ///     v        v        v
// ///   (2,0) -> (2,1) -> (2,2)
// #[test]
// fn grid3x3() {
//     let mk = |ret: ExprTyped| {
//         Program::Body(lets![
//             "00" ; B!() ;= flip!(1/2);
//             "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
//             "02" ; B!() ;= ite!( ( b!(@anf "01")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
//             "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
//             "20" ; B!() ;= ite!( ( not!("10") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );

//             "11" ; B!() ;=
//                 ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
//                 ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
//                 ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
//                                                           flip!(1/11) ))))));

//             "21" ; B!() ;=
//                 ite!(( b!((  b!(@anf "20")) && (  b!(@anf "11"))) ) ? ( flip!(2/7) ) : (
//                 ite!(( b!((  b!(@anf "20")) && (not!("11"))) ) ? ( flip!(2/8) ) : (
//                 ite!(( b!((  not!("20")) && (  b!(@anf "11"))) ) ? ( flip!(2/9) ) : (
//                                                           flip!(2/11) ))))));

//             "12" ; B!() ;=
//                 ite!(( b!((  b!(@anf "11")) && (  b!(@anf "02"))) ) ? ( flip!(6/7) ) : (
//                 ite!(( b!((  b!(@anf "11")) && (not!("02"))) ) ? ( flip!(6/8) ) : (
//                 ite!(( b!((  not!("11")) && (  b!(@anf "02"))) ) ? ( flip!(6/9) ) : (
//                                                           flip!(6/11) ))))));

//             "22" ; B!() ;=
//                 ite!(( b!((  b!(@anf "21")) && (  b!(@anf "12"))) ) ? ( flip!(3/7) ) : (
//                 ite!(( b!((  b!(@anf "21")) && (not!("12"))) ) ? ( flip!(3/8) ) : (
//                 ite!(( b!((  not!("21")) && (  b!(@anf "12"))) ) ? ( flip!(8/9) ) : (
//                                                           flip!(9/11) ))))));
//             ...? ret ; B!()
//         ])
//     };
//     check_exact1("grid3x3/exact/00", 0.500000000, &mk(b!("00")));
//     check_exact1("grid3x3/exact/01", 0.291666667, &mk(b!("01")));
//     check_exact1("grid3x3/exact/10", 0.183333333, &mk(b!("10")));
//     check_exact1("grid3x3/exact/02", 0.274305556, &mk(b!("02")));
//     check_exact1("grid3x3/exact/20", 0.193888889, &mk(b!("20")));
//     check_exact1("grid3x3/exact/11", 0.102927589, &mk(b!("11")));
//     check_exact1("grid3x3/exact/12", 0.599355085, &mk(b!("12")));
//     check_exact1("grid3x3/exact/21", 0.199103758, &mk(b!("21")));
//     check_exact1("grid3x3/exact/22", 0.770263904, &mk(b!("22")));
// }
