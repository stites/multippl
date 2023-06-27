use crate::annotate::grammar::*;
use crate::annotate::LabelEnv;
use crate::data::CompileError;
use crate::data::CompileError::Generic;
use crate::grammar::*;
use crate::typeinf::grammar::ProgramInferable;
use crate::typeinf::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::uniquify::grammar::*;
use crate::*;
use ::core::fmt::{Debug, Formatter};
use itertools::*;
// use grammar::*;
use rsdd::builder::bdd_plan::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;

use core::hash::Hash;
use std::collections::{HashMap, HashSet};
use tracing::*;

pub struct InsertionEnv {
    cuts: Vec<NamedVar>,
    current_cut: Option<NamedVar>,
    _gen_unq: u64, // FIXME reintroduce when sampling from a tuple's position.
}
impl InsertionEnv {
    pub fn new(cuts: Vec<NamedVar>, mx_uid: MaxUniqueId) -> Self {
        Self {
            cuts,
            current_cut: None,
            _gen_unq: mx_uid.0,
        }
    }
    fn _fresh_uniq(&mut self) -> UniqueId {
        // we start with a known symbol, so we increment before returning the symbol
        self._gen_unq += 1;
        UniqueId(self._gen_unq)
    }
    fn _fresh_name(&mut self) -> (UniqueId, String) {
        let sym = self._fresh_uniq();
        (sym, format!("__s#{}", sym.0))
    }
    fn _fresh_anf(&mut self) -> Var {
        let (uid, s) = self._fresh_name();
        Var::Named(NamedVar { id: uid, name: s })
    }
    pub fn sample_expr(&mut self, e: &EExprAnn) -> EExprAnn {
        use crate::grammar::EExpr::*;
        match e {
            // EAnf(_, a) => e.clone(),
            // EPrj(_, i, a) => e.clone(),
            // EFst(_, a) => e.clone(),
            // ESnd(_, a) => e.clone(),
            // EProd(v, az) => e.clone(),
            ELetIn(v, s, ebound, ebody) => {
                let cut = self.current_cut.as_ref().unwrap();
                println!("> {:?} ?= {:?} == {}", cut, v, cut.id() == v.id());
                println!("{:?}", e);
                if cut.id() == v.id() {
                    let smpl = ESample((), Box::new(SExpr::SExact((), ebound.clone())));
                    let fin = ELetIn(v.clone(), s.to_string(), Box::new(smpl), ebody.clone());
                    println!(" ==> {:?}", fin);
                    fin
                } else {
                    ELetIn(
                        v.clone(),
                        s.to_string(),
                        Box::new(self.sample_expr(ebound)),
                        Box::new(self.sample_expr(ebody)),
                    )
                }
            }
            //     debug!("{:?} ?= {:?} == {}", self.cut, v, false);
            //     let body = match (&**ebody, self.cut.clone()) {
            //         (EAnf((), a), CompoundQuery(0)) => {
            //             let (uid, s) = self.fresh_name();
            //             let nvar = NamedVar {
            //                 id: uid,
            //                 name: s.to_string(),
            //             };
            //             let to_sample = EAnf((), a.clone());
            //             let smpl = ESample((), Box::new(to_sample));
            //             let body = EAnf((), Box::new(Anf::AVar(nvar.clone(), s.to_string())));
            //             ELetIn(nvar, s.to_string(), Box::new(smpl), Box::new(body))
            //         }
            //         (EProd((), az), CompoundQuery(bindix)) => {
            //             let mut newazs = vec![];
            //             let (uid, s) = self.fresh_name();
            //             let mut sampled_anf = None;
            //             let nvar = NamedVar {
            //                 id: uid.clone(),
            //                 name: s.clone(),
            //             };
            //             for (i, a) in az.iter().enumerate() {
            //                 if i == bindix {
            //                     sampled_anf = Some(a);
            //                     newazs.push(Anf::AVar(nvar.clone(), s.clone()));
            //                 } else {
            //                     newazs.push(a.clone());
            //                 }
            //             }

            //             let to_sample = EAnf((), Box::new(sampled_anf.unwrap().clone()));
            //             let smpl = ESample((), Box::new(to_sample));
            //             let body = EProd((), newazs);
            //             ELetIn(nvar.clone(), s.to_string(), Box::new(smpl), Box::new(body))
            //         }
            //         _ => self.sample_expr(ebody),
            //     };
            //     ELetIn(
            //         v.clone(),
            //         s.to_string(),
            //         Box::new(self.sample_expr(ebound)),
            //         Box::new(body),
            //     )
            // }
            // },
            EIte(v, cond, t, f) => {
                let ts = self.sample_expr(t);
                let fs = self.sample_expr(f);
                EIte(v.clone(), cond.clone(), Box::new(ts), Box::new(fs))
            }
            ESample((), e) => ESample((), Box::new(self.sample_sexpr(&*e))),
            _ => e.clone(),
        }
    }
    pub fn sample_sexpr(&mut self, e: &SExprAnn) -> SExprAnn {
        use crate::grammar::SExpr::*;
        todo!()
    }

    pub fn apply_cut(&mut self, p: &ProgramAnn) -> ProgramAnn {
        match p {
            Program::SBody(b) => todo!(),
            Program::EBody(b) => Program::EBody(self.sample_expr(b)),
        }
    }
    pub fn apply_cuts(&mut self, p: &ProgramAnn) -> ProgramAnn {
        let cuts = self.cuts.clone();
        cuts.iter().fold(p.clone(), |fin, cut| {
            self.current_cut = Some(cut.clone());
            self.apply_cut(&fin)
        })
    }
}
mod upcast {
    use super::*;

    pub fn upcast_anf<X: Clone, Y>(a: &AnfAnn<X>) -> AnfInferable<X>
    where
        AVarExt<X>: ξ<Annotated, Ext = NamedVar> + ξ<Inferable, Ext = Option<Y>>,
        AValExt<X>: ξ<Annotated, Ext = ()> + ξ<Inferable, Ext = ()>,
    {
        use crate::grammar::Anf::*;
        match a {
            AVar(i, s) => AVar(None, s.to_string()),
            AVal(_, b) => AVal((), b.clone()),
            And(bl, br) => And(Box::new(upcast_anf(bl)), Box::new(upcast_anf(br))),
            Or(bl, br) => Or(Box::new(upcast_anf(bl)), Box::new(upcast_anf(br))),
            Neg(bl) => Neg(Box::new(upcast_anf(bl))),
            _ => todo!(),
        }
    }
    pub fn upcast_anfs<Val: Clone, Ty>(anfs: &[AnfAnn<Val>]) -> Vec<AnfInferable<Val>>
    where
        AVarExt<Val>: ξ<Annotated, Ext = NamedVar> + ξ<Inferable, Ext = Option<Ty>>,
        AValExt<Val>: ξ<Annotated, Ext = ()> + ξ<Inferable, Ext = ()>,
    {
        anfs.iter().map(|a| upcast_anf(a)).collect()
    }

    pub fn upcast_eexpr(e: &EExprAnn) -> EExprInferable {
        use EExpr::*;
        match e {
            EAnf(_, a) => EAnf((), Box::new(upcast_anf(a))),
            EPrj(_ty, i, a) => EPrj(None, *i, Box::new(upcast_anf(a))),
            EProd(_ty, anfs) => EProd(None, upcast_anfs(anfs)),
            ELetIn(_ty, s, ebound, ebody) => ELetIn(
                None,
                s.clone(),
                Box::new(upcast_eexpr(ebound)),
                Box::new(upcast_eexpr(ebody)),
            ),
            EIte(_ty, cond, t, f) => EIte(
                None,
                Box::new(upcast_anf(cond)),
                Box::new(upcast_eexpr(t)),
                Box::new(upcast_eexpr(f)),
            ),
            EFlip(_, param) => EFlip((), *param),
            EObserve(_, a) => {
                let anf = upcast_anf(a);
                EObserve((), Box::new(anf))
            }
            ESample(_, e) => ESample((), Box::new(upcast_sexpr(e))),
        }
    }

    pub fn upcast_sexpr(e: &SExprAnn) -> SExprInferable {
        use crate::grammar::SExpr::*;
        match e {
            SAnf(_, a) => SAnf((), Box::new(upcast_anf(a))),
            SSeq(_, e0, e1) => SSeq((), Box::new(upcast_sexpr(e0)), Box::new(upcast_sexpr(e1))),
            SLetIn(nvar, s, ebound, ebody) => SLetIn(
                None,
                s.clone(),
                Box::new(upcast_sexpr(ebound)),
                Box::new(upcast_sexpr(ebody)),
            ),
            SIte(_ty, cond, t, f) => SIte(
                None,
                Box::new(upcast_anf(cond)),
                Box::new(upcast_sexpr(t)),
                Box::new(upcast_sexpr(f)),
            ),
            SBern(dv, param) => SBern((), Box::new(upcast_anf(param))),
            SDiscrete(dv, ps) => SDiscrete((), upcast_anfs(ps)),
            SUniform(dv, lo, hi) => {
                SUniform((), Box::new(upcast_anf(lo)), Box::new(upcast_anf(hi)))
            }
            SNormal(dv, mean, var) => {
                SNormal((), Box::new(upcast_anf(mean)), Box::new(upcast_anf(var)))
            }
            SBeta(dv, a, b) => SBeta((), Box::new(upcast_anf(a)), Box::new(upcast_anf(b))),
            SDirichlet(dv, ps) => SDirichlet((), upcast_anfs(ps)),
            SObserve(_, a, e) => SObserve((), Box::new(upcast_anf(a)), Box::new(upcast_sexpr(e))),
            SExact(_, e) => SExact((), Box::new(upcast_eexpr(e))),
        }
    }

    pub fn upcast(p: &ProgramAnn) -> ProgramInferable {
        match p {
            Program::EBody(e) => Program::EBody(upcast_eexpr(e)),
            Program::SBody(e) => Program::SBody(upcast_sexpr(e)),
        }
    }
}

// technically, we can run this on a ProgramUniq and go backwards to get the /true/ annotated user program
pub fn insert_sample_statements(p: &ProgramInferable) -> ProgramInferable {
    upcast::upcast(&insert_sample_statements_ann(&p))
}

pub fn insert_sample_statements_stats(p: &ProgramInferable) -> (ProgramInferable, Vec<NamedVar>) {
    let (p, mx_id) = prelude(p);
    let (pann, cuts) = insert_sample_statements_h(&p, mx_id);
    (upcast::upcast(&pann), cuts)
}

// technically, we can run this on a ProgramUniq and go backwards to get the /true/ annotated user program
pub fn insert_sample_statements_ann(p: &ProgramInferable) -> ProgramAnn {
    let (p, mx_id) = prelude(p);
    insert_sample_statements_h(&p, mx_id).0
}

// technically, we can run this on a ProgramUniq and go backwards to get the /true/ annotated user program
pub fn prelude(p: &ProgramInferable) -> (ProgramAnn, MaxUniqueId) {
    use crate::analysis::*;
    let (puniq, mx_id) = crate::uniquify::pipeline(p).unwrap();
    let mut lenv = LabelEnv::new();
    let pann = lenv.annotate(&puniq).unwrap().0;
    (pann, mx_id)
}

pub fn insert_sample_statements_h(
    pann: &ProgramAnn,
    mx_id: MaxUniqueId,
) -> (ProgramAnn, Vec<NamedVar>) {
    use crate::analysis::*;
    let deps = dependencies::DependencyEnv::new().scan(&pann);
    println!("deps: {:?}", deps);
    let g = hypergraph::build_graph(&deps);
    println!("g:    {:?}", g);
    let cuts = crate::analysis::hypergraph::cutset(&g);
    println!("cuts: {:?}", cuts);
    let mut env = InsertionEnv::new(cuts.clone(), mx_id);
    (env.apply_cuts(&pann), cuts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::annotate::grammar::named;
    use crate::data::*;
    use crate::grammar::*;
    use crate::typecheck::grammar::{EExprTyped, ProgramTyped};
    use crate::typecheck::typecheck;
    use crate::*;
    use tracing::*;
    use tracing_test::traced_test;

    #[test]
    pub fn test_trivial_program() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= b!("x");
           ...? b!("y")
        ]);
        let pann = insert_sample_statements_ann(&p);
        let p1_expected = program!(lets![
            "x" ;= sample!(flip!(1/3));
            "y" ;= var!("x");
           ...? b!("y")
        ]);
        let pann_expected = crate::annotate::pipeline(&p1_expected).unwrap().0;
        assert_eq!(&pann, &pann_expected);
        let pann10 = insert_sample_statements_ann(&p);
        assert_eq!(&pann, &pann10);
    }

    #[test]
    pub fn test_alias() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= b!("x");
            "z" ;= b!("y");
           ...? b!("z")
        ]);
        let pann = insert_sample_statements_ann(&p);
        let p_expected = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= sample!(b!("x"));
            "z" ;= b!("y");
           ...? b!("z")
        ]);
        let pann_expected = crate::annotate::pipeline(&p_expected).unwrap().0;
        assert_eq!(&pann, &pann_expected);
    }

    #[test]
    pub fn test_boolean_operator() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= b!("x" && "y");
            "q" ;= flip!(1/3);
            "w" ;= b!("q" && "z");
           ...? b!("z")
        ]);
        let pann = insert_sample_statements_ann(&p);
        let p_expected = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= sample!(b!("x" && "y"));
            "q" ;= flip!(1/3);
            "w" ;= b!("q" && "z");
           ...? b!("z")
        ]);
        let pann_expected = crate::annotate::pipeline(&p_expected).unwrap().0;
        assert_eq!(&pann, &pann_expected);
    }

    #[test]
    pub fn test_tuples() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "t" ;= b!("x", "y");
            "f" ;= fst!("t");
           ...? b!("t")
        ]);
        let pann = insert_sample_statements_ann(&p);
        let p_expected = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "t" ;= sample!(b!("x", "y"));
            "f" ;= fst!("t");
           ...? b!("t")
        ]);
        let pann_expected = crate::annotate::pipeline(&p_expected).unwrap().0;
        assert_eq!(&pann, &pann_expected);
    }

    #[test]
    pub fn test_2x2_triu() {
        let grid2x2_triu = program!(lets![
            "00" ;= flip!(1/2);
            "01" ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            ...? b!("01", "10")
        ]);
        let pann = insert_sample_statements_ann(&grid2x2_triu);
        let p_expected = program!(lets![
            "00" ;= sample!(flip!(1/2));
            "01" ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            ...? b!("01", "10")
        ]);
        let pann_expected = crate::annotate::pipeline(&p_expected).unwrap().0;
        assert_eq!(&pann, &pann_expected);
    }

    #[test]
    #[ignore = "should go in hypergraph: there are no meaningful cuts here"]
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
        let pann = insert_sample_statements_ann(&grid2x2_tril);
        let p_expected = program!(lets![
            "01" ;= flip!(1/3);
            "10" ;= flip!(1/4);
            "11" ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(3/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(3/9) ) : (
                                                          flip!(3/11) ))))));
            ...? b!("11")
        ]);
        let pann_expected = crate::annotate::pipeline(&p_expected).unwrap().0;
        assert_eq!(&pann, &pann_expected);
    }

    #[test]
    #[ignore = "write this in hypergraph first"]
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
        let (pann, mx) = prelude(&grid2x2);
        let pann = insert_sample_statements_h(&pann, mx).0;
        let pann = insert_sample_statements_h(&pann, mx).0;
        let p_expected = program!(lets![
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
        let pann_expected = crate::annotate::pipeline(&p_expected).unwrap().0;
        assert_eq!(&pann, &pann_expected);
    }

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
