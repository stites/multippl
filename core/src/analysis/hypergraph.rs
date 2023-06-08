use crate::analysis::dependencies;
use crate::analysis::dependencies::*;
use crate::annotate;
use crate::annotate::grammar::*;
use crate::data::CompileError;
use crate::data::CompileError::Generic;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use core::fmt::{Debug, Formatter};
use itertools::*;
// use grammar::*;
use rsdd::builder::bdd_plan::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;

use rsdd::util::hypergraph::{hg2dt, Cluster, ClusterGraph, Edge, Hypergraph, Rank};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use tracing::*;

// FIXME: need to remove the edge_cutsets code from rsdd...
pub fn top_k_cuts(cuts: &Vec<(NamedVar, Rank)>, n: usize) -> Vec<NamedVar> {
    cuts.iter().map(|(b, r)| b.clone()).take(n).collect()
}

pub fn build_graph(deps: &DependenceMap) -> ClusterGraph<NamedVar> {
    let mut g: ClusterGraph<NamedVar> = ClusterGraph::default();
    let mut edges: HashMap<NamedVar, HashSet<Cluster<NamedVar>>> = HashMap::new();
    for family in deps.family_iter() {
        debug!("family: {:?}", family);
        let cluster = Cluster(Dep::vars(&family));
        debug!("-> cluster: {:?}", cluster);
        g.insert_vertex(cluster.clone());
        for var in &cluster.0 {
            match edges.get_mut(&var) {
                None => {
                    edges.insert(var.clone(), HashSet::from([cluster.clone()]));
                }
                Some(fams) => {
                    fams.insert(cluster.clone());
                }
            }
        }
    }
    for edge in edges.values() {
        debug!("edge: {:?}", edge);
        g.insert_edge(Edge(edge.clone()));
    }
    g.rebuild_intersections();
    g
}

pub fn cutset(g: &ClusterGraph<NamedVar>) -> Vec<NamedVar> {
    hg2dt(g).cutset().into_iter().collect()
}

pub fn pipeline(p: &crate::ProgramInferable) -> ClusterGraph<NamedVar> {
    let p = annotate::pipeline(&p).unwrap().0;
    let deps = DependencyEnv::new().scan(&p);
    build_graph(&deps)
}

#[cfg(test)]
#[allow(unused_mut)]
#[allow(unused_must_use)]
#[allow(unused_assignments)]
mod tests {
    use super::*;
    use crate::annotate::grammar::named;
    use crate::data::*;
    use crate::grammar::*;
    use crate::grammar_macros::*;
    use crate::typecheck::grammar::{EExprTyped, ProgramTyped};
    use crate::typecheck::typecheck;
    use crate::*;
    use tracing::*;
    use tracing_test::traced_test;

    fn named_to_cluster(xs: &[&NamedVar]) -> Cluster<NamedVar> {
        Cluster(xs.iter().map(Clone::clone).cloned().collect())
    }

    macro_rules! assert_clusters {
        ($g:ident : $cvar:expr) => {{
            let span = tracing::span!(tracing::Level::DEBUG, "assert_clusters");
            let _guard = span.enter();
            let cs = $g.vertices();
            debug!("{:?} in the following?", $cvar);
            for c in cs {
                debug!("vertex: {:?}", c);
            }
            assert!(cs.contains(&$cvar), "expected {:?} in hypergraph. found: {:?}", $cvar, cs);
        }};
        ($g:ident : $xvar:expr $(, $var:expr)+ $(,)?) => {{
            assert_clusters!($g : $xvar);
            $(
            assert_clusters!($g : $var);
            )+
        }};
        ($g:ident, vars: $xvar:expr) => {{
            assert_clusters!($g : named_to_cluster($xvar));
        }};
        ($g:ident, vars : $xvar:expr $(, $var:expr)+ $(,)?) => {{
            assert_clusters!($g, vars : $xvar);
            $(
            assert_clusters!($g, vars : $var);
            )+
        }};
    }

    macro_rules! assert_edges {
        (@ $n2e:ident { $var:expr => $deps:expr }) => {{
            let span = tracing::span!(tracing::Level::DEBUG, "assert_edges");
            let _guard = span.enter();

            let found = $n2e.get(&$var).unwrap_or_else(|| {
                panic!(
                    "expected singleton cluster {:?} in graph. Found: {:?}",
                    $var,
                    $n2e.keys()
                )
            });
            let e0 = Edge($deps.iter().map(|x| named_to_cluster(x)).collect());
            assert!(found.contains(&e0), "{:?} not found in {:?} entry. Got: {:?}", e0, $var, found);
        }};

        ($g:ident { $var0:expr => $dep0:expr $(, $var:expr => $deps:expr)* $(,)?}) => {{
            let n2e = &$g.intersections;
            debug!("names-to-edges: {:?}", n2e);
            let keys : HashSet<&NamedVar> = n2e.keys().collect();
            let mut allvars = HashSet::from([&$var0]);
            assert_edges!(@ n2e { $var0 => $dep0 });
            $(
                allvars.insert(&$var);
                assert_edges!(@ n2e { $var => $deps });
            )*
            assert_eq!(keys, allvars, "expected assertions to cover all found keys (on the left)");
            assert_eq!($g.hyperedges().count(), allvars.len(), "expected one edge per named variable");
        }};
    }

    type G = ClusterGraph<NamedVar>;
    macro_rules! tests_for_program {
        ($prog:expr; $($(#[ignore=$ignore:literal])? $(#[debug=$debug:literal])? $name:ident: $test:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let mut ignore = false;
                $( ignore = $ignore; )?
                if !ignore {
                    let mut debug = true;
                    $( debug = $debug; )?
                    if debug {
                        let _ = crate::utils::enable_traced_test();
                    }
                    let g : ClusterGraph<NamedVar> = pipeline(&$prog);
                    ($test)(g);
                }
            }
        )*
        }
    }

    tests_for_program! {
        program!(lets![
            "x" ;= flip!(1/3);
           ...? b!("x")
        ]);
        simple_program_hypergraph: |g: G| {
            let xvar = named(0, "x");
            assert_clusters!(g, vars: &[&named(0, "x")]);
            assert_eq!(g.vertices().len(), 1);
            debug!("{}", g.graph.print());
            assert_edges!(g { xvar => [[&xvar]] } );
        },
        #[ignore=true]
        simple_program_cuts: |g: G| {
            let ecs = cutset(&g);
            assert_eq!(ecs.len(), 1);
        },
    }

    tests_for_program! {
        program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= b!("x");
            "z" ;= b!("y");
           ...? b!("z")
        ]);
        alias_hypergraph: |g: G| {
            let xvar = named(0, "x");
            let yvar = named(2, "y");
            let zvar = named(3, "z");

            assert_clusters!(
                g,
                vars: &[&xvar],
                &[&yvar, &xvar],
                &[&zvar, &yvar],
            );

            assert_edges!(g {
                xvar => [vec![&xvar], vec![&xvar, &yvar]],
                yvar => [vec![&yvar, &zvar], vec![&xvar, &yvar]],
                zvar => [vec![&yvar, &zvar]],
            });
        },
        #[ignore=true]
        alias_cuts: |g: G| {
            let xvar = named(0, "x");
            let yvar = named(2, "y");
            let zvar = named(3, "z");
            let vec = g.edgecuts_sorted();
            // let ecs: HashMap<NamedVar, Rank> = vec.clone().into_iter().collect();
            let ecs = cutset(&g);
            println!("{:?}", ecs);
            // assert_eq!(ecs.get(&xvar).unwrap(), &Rank(1));
            // assert_eq!(ecs.get(&yvar).unwrap(), &Rank(2));
            // assert_eq!(ecs.get(&zvar).unwrap(), &Rank(1));
            assert_eq!(ecs, vec![yvar]);
        },
    }
    tests_for_program! {
        program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= b!("x" && "y");
            "q" ;= flip!(1/3);
            "w" ;= b!("q" && "z");
           ...? b!("z")
        ]);
        boolean_operator_hypergraph: |g: G| {
            let xvar = named(0, "x");
            let yvar = named(2, "y");
            let zvar = named(4, "z");
            let qvar = named(5, "q");
            let wvar = named(7, "w");

            assert_clusters!(
                g,
                vars: &[&xvar],
                &[&yvar],
                &[&xvar, &yvar, &zvar],
                &[&qvar],
                &[&qvar, &zvar, &wvar]
            );

            assert_edges!(g {
                xvar => [vec![&xvar], vec![&xvar, &yvar, &zvar]],
                yvar => [vec![&yvar], vec![&xvar, &yvar, &zvar]],
                zvar => [[&xvar, &yvar, &zvar], [&qvar, &zvar, &wvar]],
                qvar => [vec![&qvar], vec![&qvar, &zvar, &wvar]],
                wvar => [[&qvar, &zvar, &wvar]]
            });
        },
        boolean_operator_cuts: |g: G| {
            let xvar = named(0, "x");
            let yvar = named(2, "y");
            let zvar = named(4, "z");
            let qvar = named(5, "q");
            let wvar = named(7, "w");

            let vec = g.edgecuts_sorted();
            let ecs: HashMap<NamedVar, Rank> = vec.clone().into_iter().collect();
            println!("{:?}", ecs);
            assert_eq!(ecs.get(&xvar).unwrap(), &Rank(1));
            assert_eq!(ecs.get(&yvar).unwrap(), &Rank(1));
            assert_eq!(ecs.get(&zvar).unwrap(), &Rank(2));
            assert_eq!(ecs.get(&qvar).unwrap(), &Rank(1));
            assert_eq!(ecs.get(&wvar).unwrap(), &Rank(1));
            assert_eq!(vec[0].0, zvar);
        },
    }
    tests_for_program! {
        program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "t" ;= b!("x", "y");
            "f" ;= fst!("t");
           ...? b!("t")
        ]);
        tuples_hypergraph: |g: G| {
            let xvar = named(0, "x");
            let yvar = named(2, "y");
            let tvar = named(4, "t");
            let fvar = named(5, "f");

            assert_clusters!(
                g,
                vars: &[&xvar],
                &[&yvar],
                &[&xvar, &yvar, &tvar],
                &[&tvar, &fvar]
            );

            assert_edges!(g {
                xvar => [vec![&xvar], vec![&xvar, &yvar, &tvar]],
                yvar => [vec![&yvar], vec![&xvar, &yvar, &tvar]],
                tvar => [vec![&xvar, &yvar, &tvar], vec![&fvar, &tvar]],
                fvar => [[&fvar, &tvar]]
            });
        },
        tuples_cuts: |g: G| {
            let xvar = named(0, "x");
            let yvar = named(2, "y");
            let tvar = named(4, "t");
            let fvar = named(5, "f");

            let vec = g.edgecuts_sorted();
            let ecs: HashMap<NamedVar, Rank> = vec.clone().into_iter().collect();
            println!("{:?}", ecs);
            assert_eq!(ecs.get(&xvar).unwrap(), &Rank(1));
            assert_eq!(ecs.get(&yvar).unwrap(), &Rank(1));
            assert_eq!(ecs.get(&tvar).unwrap(), &Rank(2));
            assert_eq!(ecs.get(&fvar).unwrap(), &Rank(1));
            assert_eq!(vec[0].0, tvar);
        },
    }

    tests_for_program! {
        program!(lets![
           "x" ;= flip!(1/3);
           "s" ;= sample!(var!("x"));
           ...? b!("s")
        ]);
        sample_statements_cut_the_hypergraph: |g: G| {
            let xvar = named(0, "x");
            let svar = named(2, "s");

            assert_clusters!(g, vars: &[&xvar], &[&svar]);
            assert_edges!(g {
                xvar => [[&xvar]],
                svar => [[&svar]]
            });
        },
        sample_statement_cuts: |g: G| {
            let xvar = named(0, "x");
            let svar = named(2, "s");

            let vec = g.edgecuts_sorted();
            let ecs: HashMap<NamedVar, Rank> = vec.clone().into_iter().collect();
            println!("{:?}", ecs);
            assert_eq!(ecs.get(&xvar).unwrap(), &Rank(1));
            assert_eq!(ecs.get(&svar).unwrap(), &Rank(1));
        },
    }

    tests_for_program! {
        program!(lets![
            "x" ;= flip!(1/5);
            "y" ;= flip!(1/5);
            "z" ;= flip!(1/5);
            "i" ;= ite!(
                if ( var!("x") )
                then { sample!(var!("y")) }
                else { var!("z") });
            ...? b!("y")
        ]);
        sampled_ite_hypergraph: |g: G| {
            let xvar = named(0, "x");
            let yvar = named(2, "y");
            let zvar = named(4, "z");
            let ivar = named(6, "i");
            assert_clusters!(
                g,
                vars: &[&xvar],
                &[&yvar],
                &[&zvar],
                &[&ivar, &xvar, &zvar]
            );
            assert_edges!(g {
                xvar => [vec![&xvar], vec![&ivar, &xvar, &zvar]],
                yvar => [vec![&yvar]],
                zvar => [vec![&zvar], vec![&ivar, &xvar, &zvar]],
                ivar => [[&ivar, &xvar, &zvar]]
            });
        },
        sampled_ite_cuts: |g: G| {
            let xvar = named(0, "x");
            let yvar = named(2, "y");
            let zvar = named(4, "z");
            let ivar = named(6, "i");

            let vec = g.edgecuts_sorted();
            let ecs: HashMap<NamedVar, Rank> = vec.clone().into_iter().collect();
            println!("{:?}", ecs);
            assert_eq!(ecs.get(&xvar).unwrap(), &Rank(2));
            assert_eq!(ecs.get(&yvar).unwrap(), &Rank(1));
            assert_eq!(ecs.get(&zvar).unwrap(), &Rank(2));
            assert_eq!(ecs.get(&ivar).unwrap(), &Rank(2));
            assert_eq!(vec[3].0, yvar);
        },
    }

    tests_for_program! {
        program!(lets![
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
        grid_2x2_hypergraph: |g: G| {
            let var00 = named(0, "00");
            let var01 = named(2, "01");
            let var10 = named(5, "10");
            let var11 = named(8, "11");
            assert_clusters!(
                g,
                vars: &[&var00],
                &[&var00, &var10],
                &[&var00, &var01],
                &[&var00, &var01, &var10, &var11]
            );
            assert_edges!(g {
                var00 => [vec![&var00], vec![&var00, &var10], vec![&var00, &var01], vec![&var00, &var01, &var11]],
                var01 => [vec![&var00, &var01], vec![&var00, &var01, &var11]],
                var10 => [vec![&var00, &var10], vec![&var00, &var01, &var11]],
                var11 => [[&var00, &var01, &var10, &var11]]
            });
        },
        grid_2x2_cuts: |g: G| {
            let var00 = named(0, "00");
            let var01 = named(2, "01");
            let var10 = named(5, "10");
            let var11 = named(8, "11");

            let vec = cutset(&g);
            println!("{:?}", vec);
            // assert_eq!(ecs.get(&var00).unwrap(), &Rank(1000));
            // assert_eq!(ecs.get(&var01).unwrap(), &Rank(1000));
            // assert_eq!(ecs.get(&var10).unwrap(), &Rank(1000));
            // assert_eq!(ecs.get(&var11).unwrap(), &Rank(1000));
            todo!()
        },
    }
}
