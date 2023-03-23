use crate::annotate::grammar::*;
use crate::grammar::*;

use std::collections::{HashMap, HashSet};
use tracing::*;

pub struct DependencyEnv {
    dependencies: HashMap<NamedVar, HashSet<NamedVar>>,
}
impl DependencyEnv {
    pub fn new() -> Self {
        Self {
            dependencies: Default::default(),
        }
    }
    fn scan_anf(&self, a: &AnfAnn) -> HashSet<NamedVar> {
        use Anf::*;
        match a {
            AVar(nv, s) => HashSet::from([nv.clone()]),
            AVal((), _) => HashSet::new(),
            And(bl, br) => {
                let mut vs = self.scan_anf(bl);
                vs.extend(self.scan_anf(br));
                vs
            }
            Or(bl, br) => {
                let mut vs = self.scan_anf(bl);
                vs.extend(self.scan_anf(br));
                vs
            }
            Neg(bp) => self.scan_anf(bp),
        }
    }
    fn scan_expr(&mut self, e: &ExprAnn) -> HashSet<NamedVar> {
        use crate::grammar::Expr::*;
        match e {
            EAnf((), a) => self.scan_anf(a),
            // FIXME : definitely not desiarable, fix later
            // FIXME vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
            EPrj((), i, a) => self.scan_anf(a),
            EFst((), a) => self.scan_anf(a),
            ESnd((), a) => self.scan_anf(a),
            EProd((), az) => az
                .iter()
                .map(|a| self.scan_anf(a).into_iter())
                .flatten()
                .collect(),
            // FIXME ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            ELetIn(nv, s, ebound, ebody) => {
                let deps = self.scan_expr(ebound);
                self.dependencies.insert(nv.clone(), deps);
                self.scan_expr(ebody)
            }
            EIte(v, cond, t, f) => {
                let mut deps = self.scan_anf(cond);
                deps.extend(self.scan_expr(t));
                deps.extend(self.scan_expr(f));
                deps
            }
            ESample((), e) => self.scan_expr(e),
            EObserve((), a) => self.scan_anf(a),
            EFlip(_, p) => HashSet::new(),
        }
    }

    fn scan(&mut self, p: &ProgramAnn) -> HashMap<NamedVar, HashSet<NamedVar>> {
        match p {
            Program::Body(b) => {
                self.scan_expr(b);
                self.dependencies.clone()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::annotate::grammar::named;
    use crate::compile::*;
    use crate::grammar::*;
    use crate::grammar_macros::*;
    use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
    use crate::typecheck::typecheck;
    use crate::*;
    use tracing::*;
    use tracing_test::traced_test;

    macro_rules! assert_root {
        ($deps:ident : $xvar:expr $(, $var:expr)*) => {{
            let ds = $deps.get(& $xvar.clone()).unwrap_or_else(|| panic!("{:?} not found in {:?}", $xvar, $deps.keys()));
            assert_eq!(ds, &HashSet::new(), "{:?} deps should be empty", $xvar);

            $(
            let ds = $deps.get(& $var.clone()).unwrap_or_else(|| panic!("{:?} not found in {:?}", $xvar, $deps.keys()));
            assert_eq!(ds, &HashSet::new(), "{:?} deps should be empty", $var);
            )*
        }}
    }
    macro_rules! assert_family {
        ($deps:ident : $xvar:expr => $f0:expr $(, $var:expr)*) => {{
            let ds = $deps.get(& $xvar.clone()).unwrap_or_else(|| panic!("{:?} not found in {:?}", $xvar, $deps.keys()));
            let mut fam = HashSet::new();
            fam.insert($f0.clone());
            $(
            fam.insert($var.clone());
            )*
            assert_eq!(ds, &fam, "var {:?}: expected {:?}, found: {:?}", $xvar, ds, fam);
        }}
    }

    #[test]
    pub fn test_dependencies_for_simple_program() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
           ...? b!("x")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        assert_root!(deps: named(0, "x"));
        assert_eq!(deps.len(), 1);
    }

    #[test]
    pub fn test_dependencies_with_boolean_operator() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= b!("x" && "y");
            "q" ;= flip!(1/3);
            "w" ;= b!("q" && "z");
           ...? b!("z")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        let xvar = named(0, "x");
        let yvar = named(2, "y");
        let zvar = named(4, "z");
        let qvar = named(5, "q");
        let wvar = named(7, "w");
        assert_root!(deps: xvar, yvar, qvar);
        assert_family!(deps: zvar => xvar, yvar);
        assert_family!(deps: wvar => qvar, zvar);
        assert_eq!(deps.len(), 5);
    }

    #[test]
    pub fn test_dependencies_captures_tuples() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "t" ;= b!("x", "y");
            "f" ;= fst!("t");
           ...? b!("t")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        let xvar = named(0, "x");
        let yvar = named(2, "y");
        let tvar = named(4, "t");
        let fvar = named(5, "f");
        assert_root!(deps: xvar, yvar);
        assert_family!(deps: tvar => xvar, yvar);
        assert_family!(deps: fvar => tvar);
        assert_eq!(deps.len(), 4);
    }

    #[test]
    pub fn test_interaction_works_as_expected_for_samples() {
        let p = program!(lets![
           "x" ;= flip!(1/3);
           "s" ;= sample!(var!("x"));
           ...? b!("s")
        ]);
        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        let xvar = named(0, "x");
        let svar = named(2, "s");
        assert_root!(deps: xvar, svar);
        assert_eq!(deps.len(), 2);
    }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_interaction_shared_tuples_get_separated() {
    //     let shared_tuple = program!(lets![
    //        "x" ;= flip!(1/3);
    //        "y" ;= flip!(1/3);
    //        "z" ;= sample!(b!("x", "y"));
    //        ...? b!("z")
    //     ]);
    //     let g = pipeline(&shared_tuple).unwrap().0;
    //     assert_eq!(g.vertices.len(), 2);
    //     for (edge, nvar) in &g.hyperedges {
    //         println!("nvar: {:?}", nvar);
    //         println!("edge: {:?}", edge);
    //     }
    //     assert_eq!(g.hyperedges.len(), 4, "needs a tuple for each position");
    //     order_cuts(&g);
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_interaction_ite_sample() {
    //     let ite = program!(lets![
    //         "x" ;= flip!(1/5);
    //         "y" ;= ite!(
    //             if ( var!("x") )
    //             then { sample!(flip!(1/3)) }
    //             else { flip!(1/4) });
    //         ...? b!("y")
    //     ]);
    //     let g = pipeline(&ite).unwrap().0;
    //     assert_eq!(g.vertices.len(), 3);
    //     for (edge, nvar) in &g.hyperedges {
    //         println!("nvar: {:?}", nvar);
    //         println!("edge: {:?}", edge);
    //     }
    //     assert_eq!(g.hyperedges.len(), 2, "edge for each line");
    //     // let query = &g.hyperedges.last().unwrap().0;
    //     // assert_eq!(query.len(), 3, "query depends on every variable above");
    //     // order_cuts(&g);
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_interaction_ite_nested_let() {
    //     let ite_with_nested_lets = program!(lets![
    //         "x" ;= flip!(2/3);
    //         "y" ;= ite!(
    //             if ( var!("x") )
    //                 then { lets![
    //                          "q" ;= flip!(1/4);
    //                          "_" ;= observe!(b!("q" || "x"));
    //                          ...? b!("q")
    //                 ] }
    //             else { flip!(1/5) });
    //         "_" ;= observe!(b!("x" || "y"));
    //         ...? b!("x")
    //     ]);
    //     let g = pipeline(&ite_with_nested_lets).unwrap().0;
    //     assert_eq!(g.vertices.len(), 3);
    //     assert_eq!(g.hyperedges.len(), 5, "edge for each line");
    //     for (edge, nvar) in &g.hyperedges {
    //         println!("nvar: {:?}", nvar);
    //         println!("edge: {:?}", edge);
    //     }
    //     let query = &g.hyperedges.last().unwrap().0;
    //     assert_eq!(query.len(), 3, "query depends on every variable above");
    //     order_cuts(&g);
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_interaction_2x2_triu() {
    //     let grid2x2_triu = program!(lets![
    //         "00" ;= flip!(1/2);
    //         "01" ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
    //         "10" ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
    //         ...? b!("01", "10")
    //     ]);
    //     let g = pipeline(&grid2x2_triu).unwrap().0;
    //     assert_eq!(g.vertices.len(), 5);
    //     assert_eq!(g.hyperedges.len(), 3, "each var + ite");
    //     order_cuts(&g);
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_interaction_2x2_tril() {
    //     let grid2x2_tril = program!(lets![
    //         "01" ;= flip!(1/3);
    //         "10" ;= flip!(1/4);
    //         "11" ;=
    //             ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(3/7) ) : (
    //             ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(3/8) ) : (
    //             ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(3/9) ) : (
    //                                                       flip!(3/11) ))))));
    //         ...? b!("11")
    //     ]);
    //     let g = pipeline(&grid2x2_tril).unwrap().0;
    //     for (edge, nvar) in &g.hyperedges {
    //         println!("nvar: {:?}", nvar);
    //         println!("edge: {:?}", edge);
    //     }
    //     assert_eq!(g.vertices.len(), 6, "one per flip");
    //     assert_eq!(g.hyperedges.len(), 3, "one per flip + ite");
    //     let query = &g.hyperedges.last().unwrap().0;
    //     assert_eq!(query.len(), 6, "var 11 depends on every variable above");
    //     order_cuts(&g);
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_interaction_2x2_full() {
    //     let grid2x2 = program!(lets![
    //         "00" ;= flip!(1/2);
    //         "01" ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
    //         "10" ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
    //         "11" ;=
    //             ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
    //             ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
    //             ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
    //                                                       flip!(1/11) ))))));
    //         ...? b!("11")
    //     ]);
    //     let g = pipeline(&grid2x2).unwrap().0;
    //     assert_eq!(g.vertices.len(), 9);
    //     assert_eq!(g.hyperedges.len(), 4, "vertex + 3xITE");
    //     let query = &g.hyperedges.last().unwrap().0;
    //     assert_eq!(query.len(), 9, "query depends on every variable above");
    //     order_cuts(&g);
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_interaction_2x2_sampled() {
    //     let grid2x2_sampled = program!(lets![
    //         "00" ;= flip!(1/2);
    //         "01_10" ;= sample!(
    //             lets![
    //                 "01" ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
    //                 "10" ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
    //                 ...? b!("01", "10")
    //             ]);
    //         "01" ;= fst!("01_10");
    //         "10" ;= snd!("01_10");
    //         "11" ;=
    //             ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
    //             ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
    //             ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
    //                                                       flip!(1/11) ))))));
    //         ...? b!("11")
    //     ]);
    //     let g = pipeline(&grid2x2_sampled).unwrap().0;
    //     assert_eq!(g.vertices.len(), 9);
    //     for (edge, nvar) in &g.hyperedges {
    //         println!("{:?} >>> {:?}", nvar, edge);
    //     }
    //     assert_eq!(g.hyperedges.len(), 4, "every named ITE + 00");
    //     let query = &g.hyperedges.last().unwrap().0;
    //     assert_eq!(
    //         query.len(),
    //         9,
    //         "11 (last one) depends on every variable above"
    //     );
    //     order_cuts(&g);
    // }
}
