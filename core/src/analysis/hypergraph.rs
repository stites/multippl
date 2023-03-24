use crate::analysis::dependencies;
use crate::analysis::dependencies::*;
use crate::annotate;
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
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use tracing::*;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Edge<V>(HashSet<V>)
where
    V: Clone + Debug + PartialEq + Eq + Hash;
impl<V> Hash for Edge<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.0.len().hash(state);
        self.0.iter().for_each(|v| v.hash(state));
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cover<'a, V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    cover: HashSet<V>,
    edges: HashSet<&'a Edge<V>>,
}

pub trait Hypergraph {
    type Vertex;
    type EdgeIter: Iterator<Item = HashSet<Self::Vertex>>;
    fn vertices(&self) -> &HashSet<Self::Vertex>;
    fn hyperedges(&self) -> Self::EdgeIter;
    fn insert_edge(&mut self, edge: &HashSet<Self::Vertex>) -> bool;
    fn insert_vertex(&mut self, v: Self::Vertex) -> bool;
}

#[derive(Clone, Debug)]
pub struct HGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    vertices: HashSet<V>,
    hyperedges: HashSet<Edge<V>>,
}
impl<V> Default for HGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    fn default() -> Self {
        HGraph {
            vertices: Default::default(),
            hyperedges: Default::default(),
        }
    }
}
impl<V> Hypergraph for HGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    type Vertex = V;
    type EdgeIter = std::vec::IntoIter<HashSet<Self::Vertex>>;

    fn vertices(&self) -> &HashSet<V> {
        &self.vertices
    }

    fn hyperedges<'a>(&self) -> Self::EdgeIter {
        self.hyperedges
            .iter()
            .map(|x| &x.0)
            .cloned()
            .collect_vec()
            .into_iter()
    }

    /// add an edge to the hypergraph. Returns false if the edge is already in the hypergraph
    fn insert_edge(&mut self, edge: &HashSet<V>) -> bool {
        debug!("insert edge: {:?}", edge);
        self.vertices.extend(edge.clone());
        self.hyperedges.insert(Edge(edge.clone()))
    }

    /// add a vertex to the hypergraph. Returns false if the vertex is already in the hypergraph
    fn insert_vertex(&mut self, v: V) -> bool {
        self.vertices.insert(v)
    }
}
impl<V> HGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    pub fn print(&self) -> String {
        let vtxs = self.vertices.iter().map(|x| format!("{:?}", x)).join(", ");
        let edges = self
            .hyperedges
            .iter()
            .map(|x| format!("{:?}", x.0))
            .join(",\n");
        let mut s = String::from("");
        s.push_str(&format!("vertices: {{ {} }}\n", vtxs));
        s.push_str("edges:\n");
        for edge in &self.hyperedges {
            s.push_str(&format!("  | {:?}\n", edge));
        }
        s
    }

    // /// cut a vertex out of the hypergraph
    // pub fn rank_edge_cuts(&self) -> Vec<(&HashSet<V>, Rank)> {
    //     self.hyperedges
    //         .iter()
    //         .map(|edge| (edge, self.edge_cut_rank(edge)))
    //         .collect()
    // }

    // pub fn edge_cut_rank(&self, edge: &HashSet<V>) -> Rank {
    //     let nextedges = self
    //         .hyperedges
    //         .iter()
    //         .filter(|e| e != edge)
    //         .cloned()
    //         .collect_vec();
    //     let newcovers : Vec<Cover<'a, V>> = Self::edges_to_covers(&nextedges);
    //     Rank(newcovers.len())
    // }

    // fn edges_to_covers<'a> (
    //     hyperedges: &'a HashSet<Edge<V>>,
    // ) -> Cover<'a, V> {
    //     let mut overlaps: Vec<(HashSet<V>, HashSet<&Edge<V>>)> = vec![];
    //     for (edge, label) in hyperedges.iter() {
    //         let overlaps_for_edge: Vec<(usize, HashSet<V>, Vec<(&HashSet<V>, &EL)>)> = overlaps
    //             .iter()
    //             .enumerate()
    //             .filter(|(_, (oedge, _))| !oedge.is_disjoint(edge))
    //             .map(|(a, (b, c))| (a, b.clone(), c.clone()))
    //             .collect_vec();

    //         if overlaps_for_edge.is_empty() {
    //             overlaps.push((edge.clone(), vec![(edge, label)]))
    //         } else {
    //             // update a cover
    //             let mut new_cover = edge.clone();
    //             let mut new_cover_edges = vec![(edge, label)];
    //             let mut diff = 0;
    //             for (cover_ix, cover, edges) in overlaps_for_edge {
    //                 new_cover.extend(cover.clone());
    //                 new_cover_edges.extend(edges);
    //                 overlaps.remove(cover_ix - diff);
    //                 diff += 1;
    //             }
    //             let nonempty_new_cover_edges = new_cover_edges
    //                 .into_iter()
    //                 .filter(|s| !s.0.is_empty())
    //                 .collect();
    //             overlaps.push((
    //                 new_cover,
    //                 dedupe_labeled_hashset_refs(nonempty_new_cover_edges),
    //             ));
    //         }
    //     }
    //     overlaps
    // }

    // pub fn covers(&self) -> Vec<(HashSet<V>, Vec<(&HashSet<V>, &EL)>)> {
    //     Self::edges_to_covers(&self.hyperedges)
    // }
}
pub fn build_graph(deps: &Dependencies) -> HGraph<Cluster<NamedVar>> {
    let mut g = HGraph::default();
    let mut edges: HashMap<NamedVar, HashSet<Cluster<NamedVar>>> = HashMap::new();
    for family in deps.family_iter() {
        let cluster = Cluster(family.clone());
        g.insert_vertex(cluster.clone());
        for var in family {
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
    for edges in edges.values() {
        g.insert_edge(edges);
    }
    g
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cluster<V>(HashSet<V>)
where
    V: Clone + Debug + PartialEq + Eq + Hash;
impl<V> Hash for Cluster<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.0.len().hash(state);
        self.0.iter().for_each(|v| v.hash(state));
    }
}

pub fn pipeline(p: &crate::ProgramInferable) -> HGraph<Cluster<NamedVar>> {
    let p = annotate::pipeline(&p).unwrap().0;
    let deps = DependencyEnv::new().scan(&p);
    build_graph(&deps)
}

// #[derive(Clone, Copy, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
// pub struct Rank(pub usize);
// pub fn order_cuts(g: &IteractionGraph) -> Vec<(Binding, Rank)> {
//     let mut ranking = vec![];
//     let mut sorted_edges = g.rank_edge_cuts();
//     sorted_edges.sort_by(|(_, _, a), (_, _, b)| b.cmp(&a));
//     for (edges, label, rank) in sorted_edges {
//         ranking.push((label.clone(), rank));
//     }
//     ranking
// }
// pub fn top_k_cuts(cuts: &Vec<(Binding, Rank)>, n: usize) -> Vec<Binding> {
//     assert!(
//         n - 1 < cuts.len(),
//         "requested {} cuts, but only {} possible cut{} found",
//         n,
//         cuts.len(),
//         String::from(if cuts.len() == 1 { "" } else { "s" })
//     );
//     cuts.iter().map(|(b, r)| b.clone()).take(n).collect()
// }

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

    // #[test]
    // pub fn test_interaction_graph_for_simple_program() {
    //     let p = program!(lets![
    //         "x" ;= flip!(1/3);
    //        ...? b!("x")
    //     ]);
    //     let (g, uid, lbl) = pipeline(&p).unwrap();
    //     assert_eq!(g.vertices.len(), 1);
    //     assert_eq!(g.hyperedges.len(), 1);
    //     let (_, nvar) = g.hyperedges[0].clone();
    //     assert_eq!(nvar, Binding::Let(named(0, "x")), "flip edge first");
    //     let cuts = order_cuts(&g);
    //     let pann = crate::annotate::pipeline(&p).unwrap().0;
    //     let cs = top_k_cuts(&cuts, 1);
    // }

    // #[test]
    // pub fn test_interaction_graph_with_boolean_operator() {
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
    // }

    // #[test]
    // pub fn test_interaction_graph_works_with_conjoined_query() {
    //     let p = program!(lets![
    //         "x" ;= flip!(0.0);
    //         "y" ;= flip!(0.0);
    //        ...? b!("x" && "y")
    //     ]);
    //     let (g, uid, lbl) = pipeline(&p).unwrap();
    //     assert_eq!(g.vertices.len(), 2);
    //     println!("{}", g.print());
    //     assert_eq!(g.hyperedges.len(), 3, "each line + complex query");
    //     let cuts = order_cuts(&g);
    //     assert_eq!(cuts.len(), 3);
    //     let pann = crate::annotate::pipeline(&p).unwrap().0;
    //     let cs = top_k_cuts(&cuts, 1);
    //     for c in &cuts {
    //         println!("{:?}", c);
    //     }
    // }

    // #[test]
    // pub fn test_interaction_graph_captures_correct_edge_with_aliases() {
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
    // pub fn test_interaction_works_as_expected_for_samples() {
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
    // // #[traced_test]
    // // #[ignore = "expectations need to be revisited"]
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
