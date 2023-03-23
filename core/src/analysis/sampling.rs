use crate::annotate::grammar::*;
use crate::compile::CompileError;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::CompileError::Generic;
use core::fmt::{Debug, Formatter};
use itertools::*;
// use grammar::*;
use rsdd::builder::bdd_plan::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;

// use rsdd::util::hypergraph::*;
use crate::analysis::hypergraph;
use crate::analysis::hypergraph::*;
use core::hash::Hash;
use std::collections::{HashMap, HashSet};
use tracing::*;

pub struct InsertionEnv {
    cuts: Vec<Binding>,
    cut: Binding,
    gen_unq: u64,
    gen_lbl: u64,
}
impl InsertionEnv {
    pub fn new(cuts: Vec<Binding>, mx_uid: MaxUniqueId, mx_lbl: MaxVarLabel) -> Self {
        let cut = cuts.first().as_ref().unwrap().clone().to_owned();
        Self {
            cuts,
            cut,
            gen_unq: mx_uid.0,
            gen_lbl: mx_lbl.0,
        }
    }
    pub fn fresh_uniq(&mut self) -> UniqueId {
        // we start with a known symbol, so we increment before returning the symbol
        self.gen_unq += 1;
        UniqueId(self.gen_unq)
    }
    pub fn fresh_name(&mut self) -> (UniqueId, String) {
        let sym = self.fresh_uniq();
        (sym, format!("__s#{}", sym.0))
    }
    pub fn fresh_anf(&mut self) -> Var {
        let (uid, s) = self.fresh_name();
        Var::Named(NamedVar { id: uid, name: s })
    }
    pub fn sample_expr(&mut self, e: &ExprAnn) -> ExprAnn {
        use crate::grammar::Expr::*;
        use Binding::*;
        match e {
            // EAnf(_, a) => e.clone(),
            // EPrj(_, i, a) => e.clone(),
            // EFst(_, a) => e.clone(),
            // ESnd(_, a) => e.clone(),
            // EProd(v, az) => e.clone(),
            ELetIn(v, s, ebound, ebody) => match self.cut.id().map(|ix| ix == v.id()) {
                Some(true) => {
                    debug!("> {:?} ?= {:?} == {}", self.cut, v, true);
                    let smpl = ESample((), Box::new(self.sample_expr(ebound)));
                    ELetIn(v.clone(), s.to_string(), Box::new(smpl), ebody.clone())
                }
                _ => {
                    debug!("{:?} ?= {:?} == {}", self.cut, v, false);
                    let body = match (&**ebody, self.cut.clone()) {
                        (EAnf((), a), CompoundQuery(0)) => {
                            let (uid, s) = self.fresh_name();
                            let nvar = NamedVar {
                                id: uid,
                                name: s.to_string(),
                            };
                            let to_sample = EAnf((), a.clone());
                            let smpl = ESample((), Box::new(to_sample));
                            let body = EAnf((), Box::new(Anf::AVar(nvar.clone(), s.to_string())));
                            ELetIn(nvar, s.to_string(), Box::new(smpl), Box::new(body))
                        }
                        (EProd((), az), CompoundQuery(bindix)) => {
                            let mut newazs = vec![];
                            let (uid, s) = self.fresh_name();
                            let mut sampled_anf = None;
                            let nvar = NamedVar {
                                id: uid.clone(),
                                name: s.clone(),
                            };
                            for (i, a) in az.iter().enumerate() {
                                if i == bindix {
                                    sampled_anf = Some(a);
                                    newazs.push(Anf::AVar(nvar.clone(), s.clone()));
                                } else {
                                    newazs.push(a.clone());
                                }
                            }

                            let to_sample = EAnf((), Box::new(sampled_anf.unwrap().clone()));
                            let smpl = ESample((), Box::new(to_sample));
                            let body = EProd((), newazs);
                            ELetIn(nvar.clone(), s.to_string(), Box::new(smpl), Box::new(body))
                        }
                        _ => self.sample_expr(ebody),
                    };
                    ELetIn(
                        v.clone(),
                        s.to_string(),
                        Box::new(self.sample_expr(ebound)),
                        Box::new(body),
                    )
                }
            },
            EIte(v, cond, t, f) => {
                let ts = self.sample_expr(t);
                let fs = self.sample_expr(f);
                EIte(v.clone(), cond.clone(), Box::new(ts), Box::new(fs))
            }
            ESample((), e) => ESample((), Box::new(self.sample_expr(e))),
            _ => e.clone(),
        }
    }

    pub fn apply_cut(&mut self, p: &ProgramAnn) -> ProgramAnn {
        match p {
            Program::Body(b) => Program::Body(self.sample_expr(b)),
        }
    }
    pub fn apply_cuts(&mut self, p: &ProgramAnn) -> ProgramAnn {
        let cuts = self.cuts.clone();
        cuts.iter().fold(p.clone(), |fin, cut| {
            self.cut = cut.clone();
            self.apply_cut(&fin)
        })
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
    pub fn test_interaction_graph_for_simple_program() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
           ...? b!("x")
        ]);
        let (g, uid, lbl) = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 1);
        assert_eq!(g.hyperedges.len(), 1);
        let (_, nvar) = g.hyperedges[0].clone();
        assert_eq!(nvar, Binding::Let(named(0, "x")), "flip edge first");
        let cuts = order_cuts(&g);
        let pann = crate::annotate::pipeline(&p).unwrap().0;
        let cs = top_k_cuts(&cuts, 1);
        let mut env = InsertionEnv::new(cs, uid, lbl);
        let p1 = env.apply_cuts(&pann);
        let p1_expected = program!(lets![
            "x" ;= sample!(flip!(1/3));
           ...? b!("x")
        ]);
        let pann_expected = crate::annotate::pipeline(&p1_expected).unwrap().0;
        assert_eq!(&p1, &pann_expected);
    }

    #[test]
    pub fn test_interaction_graph_with_boolean_operator() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= b!("x" && "y");
           ...? b!("z")
        ]);
        let (g, uid, lbl) = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 2);
        println!("{}", g.print());
        assert_eq!(g.hyperedges.len(), 3, "edge for each line");
        for (ix, (id, s)) in [(0, "x"), (2, "y"), (4, "z")].iter().enumerate() {
            let (_, nvar) = g.hyperedges[ix].clone();
            assert_eq!(nvar, Binding::Let(named(*id, s)));
        }
        let cuts = order_cuts(&g);
        assert_eq!(cuts.len(), 3);
        let pann = crate::annotate::pipeline(&p).unwrap().0;
        let cs = top_k_cuts(&cuts, 1);
        println!("{:#?}", cuts);
        let mut env = InsertionEnv::new(cs, uid, lbl);
        let p1 = env.apply_cuts(&pann);
        let p1_expected = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= sample!(b!("x" && "y"));
           ...? b!("z")
        ]);
        let pann_expected = crate::annotate::pipeline(&p1_expected).unwrap().0;
        assert_eq!(&p1, &pann_expected);
    }

    #[test]
    pub fn test_interaction_graph_works_with_conjoined_query() {
        let p = program!(lets![
            "x" ;= flip!(0.0);
            "y" ;= flip!(0.0);
           ...? b!("x" && "y")
        ]);
        let (g, uid, lbl) = hypergraph::pipeline(&p).unwrap();
        let cuts = order_cuts(&g);
        assert_eq!(cuts.len(), 3);
        let pann = crate::annotate::pipeline(&p).unwrap().0;
        let cs = top_k_cuts(&cuts, 1);
        for c in &cuts {
            println!("{:?}", c);
        }
        let mut env = InsertionEnv::new(cs, uid, lbl);
        let p1 = env.apply_cuts(&pann);
        let p1_expected = program!(lets![
            "x" ;= flip!(0.0);
            "y" ;= flip!(0.0);
            "__s#4" ;= sample!(b!("x" && "y"));
           ...? b!("__s#4")
        ]);
        let pann_expected = crate::annotate::pipeline(&p1_expected).unwrap().0;
        assert_eq!(&p1, &pann_expected);
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_graph_captures_correct_edge_with_aliases() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= flip!(1/3);
            "a" ;= b!("x" && "y" && "z");
           ...? b!("a")
        ]);
        let g = pipeline(&p).unwrap().0;
        assert_eq!(g.vertices.len(), 3);
        assert_eq!(g.hyperedges.len(), 4, "edge for each line");
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 3, "query depends on every variable above");
        order_cuts(&g);
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_works_as_expected_for_samples() {
        let shared_var = program!(lets![
           "x" ;= flip!(1/3);
           "l" ;= sample!(var!("x"));
           "r" ;= sample!(var!("x"));
           ...? b!("r")
        ]);
        let g = pipeline(&shared_var).unwrap().0;
        assert_eq!(g.vertices.len(), 1);
        let es = g.hyperedges.clone();
        assert_eq!(g.hyperedges.len(), 3, "edge for each line");
        let n = &es[0].1;
        assert_eq!(n, &Binding::Let(named(0, "x")));
        let (e, n) = &es[1];
        assert_eq!(n, &Binding::NamedSample(named(2, "l")));
        assert_eq!(e, &HashSet::from([PlanPtr(0)]));
        let (e, n) = &es[2];
        assert_eq!(n, &Binding::NamedSample(named(3, "r")));
        assert_eq!(e, &HashSet::from([PlanPtr(0)]));
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_shared_tuples_get_separated() {
        let shared_tuple = program!(lets![
           "x" ;= flip!(1/3);
           "y" ;= flip!(1/3);
           "z" ;= sample!(b!("x", "y"));
           ...? b!("z")
        ]);
        let g = pipeline(&shared_tuple).unwrap().0;
        assert_eq!(g.vertices.len(), 2);
        for (edge, nvar) in &g.hyperedges {
            println!("nvar: {:?}", nvar);
            println!("edge: {:?}", edge);
        }
        assert_eq!(g.hyperedges.len(), 4, "needs a tuple for each position");
        order_cuts(&g);
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_ite_sample() {
        let ite = program!(lets![
            "x" ;= flip!(1/5);
            "y" ;= ite!(
                if ( var!("x") )
                then { sample!(flip!(1/3)) }
                else { flip!(1/4) });
            ...? b!("y")
        ]);
        let g = pipeline(&ite).unwrap().0;
        assert_eq!(g.vertices.len(), 3);
        for (edge, nvar) in &g.hyperedges {
            println!("nvar: {:?}", nvar);
            println!("edge: {:?}", edge);
        }
        assert_eq!(g.hyperedges.len(), 2, "edge for each line");
        // let query = &g.hyperedges.last().unwrap().0;
        // assert_eq!(query.len(), 3, "query depends on every variable above");
        // order_cuts(&g);
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_ite_nested_let() {
        let ite_with_nested_lets = program!(lets![
            "x" ;= flip!(2/3);
            "y" ;= ite!(
                if ( var!("x") )
                    then { lets![
                             "q" ;= flip!(1/4);
                             "_" ;= observe!(b!("q" || "x"));
                             ...? b!("q")
                    ] }
                else { flip!(1/5) });
            "_" ;= observe!(b!("x" || "y"));
            ...? b!("x")
        ]);
        let g = pipeline(&ite_with_nested_lets).unwrap().0;
        assert_eq!(g.vertices.len(), 3);
        assert_eq!(g.hyperedges.len(), 5, "edge for each line");
        for (edge, nvar) in &g.hyperedges {
            println!("nvar: {:?}", nvar);
            println!("edge: {:?}", edge);
        }
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 3, "query depends on every variable above");
        order_cuts(&g);
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_2x2_triu() {
        let grid2x2_triu = program!(lets![
            "00" ;= flip!(1/2);
            "01" ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            ...? b!("01", "10")
        ]);
        let g = pipeline(&grid2x2_triu).unwrap().0;
        assert_eq!(g.vertices.len(), 5);
        assert_eq!(g.hyperedges.len(), 3, "each var + ite");
        order_cuts(&g);
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_2x2_tril() {
        let grid2x2_tril = program!(lets![
            "01" ;= flip!(1/3);
            "10" ;= flip!(1/4);
            "11" ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(3/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(3/9) ) : (
                                                          flip!(3/11) ))))));
            ...? b!("11")
        ]);
        let g = pipeline(&grid2x2_tril).unwrap().0;
        for (edge, nvar) in &g.hyperedges {
            println!("nvar: {:?}", nvar);
            println!("edge: {:?}", edge);
        }
        assert_eq!(g.vertices.len(), 6, "one per flip");
        assert_eq!(g.hyperedges.len(), 3, "one per flip + ite");
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 6, "var 11 depends on every variable above");
        order_cuts(&g);
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_2x2_full() {
        let grid2x2 = program!(lets![
            "00" ;= flip!(1/2);
            "01" ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            "11" ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                          flip!(1/11) ))))));
            ...? b!("11")
        ]);
        let g = pipeline(&grid2x2).unwrap().0;
        assert_eq!(g.vertices.len(), 9);
        assert_eq!(g.hyperedges.len(), 4, "vertex + 3xITE");
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 9, "query depends on every variable above");
        order_cuts(&g);
    }

    #[test]
    #[ignore = "cross over from hypergraph.rs -- needs to be revamped for sampling test"]
    pub fn test_interaction_2x2_sampled() {
        let grid2x2_sampled = program!(lets![
            "00" ;= flip!(1/2);
            "01_10" ;= sample!(
                lets![
                    "01" ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                    "10" ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
                    ...? b!("01", "10")
                ]);
            "01" ;= fst!("01_10");
            "10" ;= snd!("01_10");
            "11" ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                          flip!(1/11) ))))));
            ...? b!("11")
        ]);
        let g = pipeline(&grid2x2_sampled).unwrap().0;
        assert_eq!(g.vertices.len(), 9);
        for (edge, nvar) in &g.hyperedges {
            println!("{:?} >>> {:?}", nvar, edge);
        }
        assert_eq!(g.hyperedges.len(), 4, "every named ITE + 00");
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(
            query.len(),
            9,
            "11 (last one) depends on every variable above"
        );
        order_cuts(&g);
    }
}
