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

    #[test]
    pub fn test_dependencies_for_simple_program() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
           ...? b!("x")
        ]);

        let p = annotate::pipeline(&p).unwrap().0;
        let deps = DependencyEnv::new().scan(&p);
        assert_eq!(deps.len(), 1);
        let xdeps = deps.get(&named(0, "x")).unwrap();
        assert_eq!(xdeps, &HashSet::new());
        // assert_eq!(g.vertices.len(), 1);
        // assert_eq!(g.hyperedges.len(), 1);
        // let (_, nvar) = g.hyperedges[0].clone();
        // assert_eq!(nvar, Binding::Let(named(0, "x")), "flip edge first");
        // let cuts = order_cuts(&g);
        // let pann = crate::annotate::pipeline(&p).unwrap().0;
        // let cs = top_k_cuts(&cuts, 1);
        // let mut env = InsertionEnv::new(cs, uid, lbl);
        // let p1 = env.apply_cuts(&pann);
        // let p1_expected = program!(lets![
        //     "x" ;= sample!(flip!(1/3));
        //    ...? b!("x")
        // ]);
        // let pann_expected = crate::annotate::pipeline(&p1_expected).unwrap().0;
        // assert_eq!(&p1, &pann_expected);
    }

    // #[test]
    // pub fn test_dependencies_graph_with_boolean_operator() {
    //     let p = program!(lets![
    //         "x" ;= flip!(1/3);
    //         "y" ;= flip!(1/3);
    //         "z" ;= b!("x" && "y");
    //        ...? b!("z")
    //     ]);
    //     let (g, uid, lbl) = pipeline(&p).unwrap();
    //     assert_eq!(g.vertices.len(), 2);
    //     println!("{}", g.print());
    //     assert_eq!(g.hyperedges.len(), 3, "edge for each line");
    //     for (ix, (id, s)) in [(0, "x"), (2, "y"), (4, "z")].iter().enumerate() {
    //         let (_, nvar) = g.hyperedges[ix].clone();
    //         assert_eq!(nvar, Binding::Let(named(*id, s)));
    //     }
    //     let cuts = order_cuts(&g);
    //     assert_eq!(cuts.len(), 3);
    //     let pann = crate::annotate::pipeline(&p).unwrap().0;
    //     let cs = top_k_cuts(&cuts, 1);
    //     println!("{:#?}", cuts);
    //     let mut env = InsertionEnv::new(cs, uid, lbl);
    //     let p1 = env.apply_cuts(&pann);
    //     let p1_expected = program!(lets![
    //         "x" ;= flip!(1/3);
    //         "y" ;= flip!(1/3);
    //         "z" ;= sample!(b!("x" && "y"));
    //        ...? b!("z")
    //     ]);
    //     let pann_expected = crate::annotate::pipeline(&p1_expected).unwrap().0;
    //     assert_eq!(&p1, &pann_expected);
    // }

    // #[test]
    // pub fn test_dependencies_graph_works_with_conjoined_query() {
    //     let p = program!(lets![
    //         "x" ;= flip!(0.0);
    //         "y" ;= flip!(0.0);
    //        ...? b!("x" && "y")
    //     ]);
    //     let (g, uid, lbl) = hypergraph::pipeline(&p).unwrap();
    //     let cuts = order_cuts(&g);
    //     assert_eq!(cuts.len(), 3);
    //     let pann = crate::annotate::pipeline(&p).unwrap().0;
    //     let cs = top_k_cuts(&cuts, 1);
    //     for c in &cuts {
    //         println!("{:?}", c);
    //     }
    //     let mut env = InsertionEnv::new(cs, uid, lbl);
    //     let p1 = env.apply_cuts(&pann);
    //     let p1_expected = program!(lets![
    //         "x" ;= flip!(0.0);
    //         "y" ;= flip!(0.0);
    //         "__s#4" ;= sample!(b!("x" && "y"));
    //        ...? b!("__s#4")
    //     ]);
    //     let pann_expected = crate::annotate::pipeline(&p1_expected).unwrap().0;
    //     assert_eq!(&p1, &pann_expected);
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_dependencies_graph_captures_correct_edge_with_aliases() {
    //     let p = program!(lets![
    //         "x" ;= flip!(1/3);
    //         "y" ;= flip!(1/3);
    //         "z" ;= flip!(1/3);
    //         "a" ;= b!("x" && "y" && "z");
    //        ...? b!("a")
    //     ]);
    //     let g = pipeline(&p).unwrap().0;
    //     assert_eq!(g.vertices.len(), 3);
    //     assert_eq!(g.hyperedges.len(), 4, "edge for each line");
    //     let query = &g.hyperedges.last().unwrap().0;
    //     assert_eq!(query.len(), 3, "query depends on every variable above");
    //     order_cuts(&g);
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_dependencies_works_as_expected_for_samples() {
    //     let shared_var = program!(lets![
    //        "x" ;= flip!(1/3);
    //        "l" ;= sample!(var!("x"));
    //        "r" ;= sample!(var!("x"));
    //        ...? b!("r")
    //     ]);
    //     let g = pipeline(&shared_var).unwrap().0;
    //     assert_eq!(g.vertices.len(), 1);
    //     let es = g.hyperedges.clone();
    //     assert_eq!(g.hyperedges.len(), 3, "edge for each line");
    //     let n = &es[0].1;
    //     assert_eq!(n, &Binding::Let(named(0, "x")));
    //     let (e, n) = &es[1];
    //     assert_eq!(n, &Binding::NamedSample(named(2, "l")));
    //     assert_eq!(e, &HashSet::from([PlanPtr(0)]));
    //     let (e, n) = &es[2];
    //     assert_eq!(n, &Binding::NamedSample(named(3, "r")));
    //     assert_eq!(e, &HashSet::from([PlanPtr(0)]));
    // }

    // #[test]
    // #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    // pub fn test_dependencies_shared_tuples_get_separated() {
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
    // pub fn test_dependencies_ite_sample() {
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
    // pub fn test_dependencies_ite_nested_let() {
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
    // pub fn test_dependencies_2x2_triu() {
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
    // pub fn test_dependencies_2x2_tril() {
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
    // pub fn test_dependencies_2x2_full() {
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
    // pub fn test_dependencies_2x2_sampled() {
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
