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
pub struct Edge<V>(HashSet<V>)
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
pub fn common_variables(clusters: &HashSet<Cluster<NamedVar>>) -> HashSet<NamedVar> {
    let mut common = clusters
        .iter()
        .map(|x| x.0.clone())
        .nth(0)
        .expect("clusters should be non-empty");
    for c in clusters.iter() {
        common = common.intersection(&c.0).cloned().collect();
    }
    common
}
impl HGraph<Cluster<NamedVar>> {
    pub fn names_to_edges(&self) -> HashMap<NamedVar, HashSet<Edge<Cluster<NamedVar>>>> {
        let mut ret: HashMap<NamedVar, HashSet<Edge<Cluster<NamedVar>>>> = HashMap::new();
        for edge in self.hyperedges() {
            for cluster in edge.iter() {
                for name in cluster.0.iter() {
                    match ret.get_mut(&name) {
                        None => {
                            ret.insert(name.clone(), HashSet::from([Edge(edge.clone())]));
                        }
                        Some(es) => {
                            es.insert(Edge(edge.clone()));
                        }
                    }
                }
            }
        }
        ret
    }
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

    macro_rules! assert_clusters {
        ($g:ident : $cvar:expr) => {{
            let cs = $g.vertices();
            assert!(cs.contains(&$cvar), "expected {:?} in hypergraph. found: {:?}", $cvar, cs);
        }};
        ($g:ident, : $xvar:expr $(, $var:expr)+) => {{
            assert_clusters!($g : $xvar);
            $(
            assert_clusters!($g : $var);
            )+
        }};
        ($g:ident, vars: $xvar:expr) => {{
            assert_clusters!($g : Cluster(HashSet::from([$xvar])));
        }};
        ($g:ident, vars : $xvar:expr $(, $var:expr)+) => {{
            assert_clusters!($g, vars : $xvar);
            $(
            assert_clusters!($g, vars : $var);
            )+
        }};
    }
    macro_rules! assert_edges {
        (@ $n2e:ident { $var:expr => $dep0:expr $(, $deps:expr)* }) => {{
            let found = $n2e.get(&$var).unwrap_or_else(|| {
                panic!(
                    "expected singleton cluster {:?} in graph. Found: {:?}",
                    $var,
                    $n2e.keys()
                )
            });
            let e0 = Edge($dep0.iter().map(|x| Cluster(HashSet::from(x.clone()))).collect());
            assert!(found.contains(&e0));
        }};

        ($g:ident { $var:expr => $dep0:expr $(, $deps:expr)* }) => {{
            let n2e = $g.names_to_edges();
            let count = 1;
            assert_edges!(@ n2e { $var => $dep0 $(, $deps:expr)* });
            assert_eq!($g.hyperedges().count(), count);
        }};
        // ($g:ident { $xvar:expr => $xdeps:expr $(, $var:expr => $deps:expr)* }) => {{
            //     let n2e = $g.names_to_edges(); // -> HashMap<NamedVar, HashSet<Edge<Cluster<NamedVar>>>> {

            //     let cs = $g.hyperedges();
            //     let mut fam = HashSet::new();
            //     fam.insert($f0.clone());
            //     $(
            //     fam.insert($var.clone());
            //     )*
            //     assert_eq!(ds, &fam, "var {:?}: expected {:?}, found: {:?}", $xvar, ds, fam);
            // }}
    }

    #[test]
    pub fn test_hypergraphs_for_simple_program() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
           ...? b!("x")
        ]);
        let g = pipeline(&p);

        let xvar = named(0, "x");
        assert_clusters!(g, vars: named(0, "x"));
        assert_eq!(g.vertices().len(), 1);
        println!("{}", g.print());

        assert_edges!(g { xvar => [[xvar]] } );
    }

    // #[test]
    // pub fn test_hypergraphs_with_boolean_operator() {
    //     let p = program!(lets![
    //         "x" ;= flip!(1/3);
    //         "y" ;= flip!(1/3);
    //         "z" ;= b!("x" && "y");
    //         "q" ;= flip!(1/3);
    //         "w" ;= b!("q" && "z");
    //        ...? b!("z")
    //     ]);
    //     let g = pipeline(&p);
    //     let xvar = named(0, "x");
    //     let yvar = named(2, "y");
    //     let zvar = named(4, "z");
    //     let qvar = named(5, "q");
    //     let wvar = named(7, "w");
    //     // assert_root!(deps: xvar, yvar, qvar);
    //     // assert_family!(deps: zvar => xvar, yvar);
    //     // assert_family!(deps: wvar => qvar, zvar);
    //     // assert_eq!(deps.len(), 5);
    // }

    // #[test]
    // pub fn test_hypergraphs_captures_tuples() {
    //     let p = program!(lets![
    //         "x" ;= flip!(1/3);
    //         "y" ;= flip!(1/3);
    //         "t" ;= b!("x", "y");
    //         "f" ;= fst!("t");
    //        ...? b!("t")
    //     ]);
    //     let p = annotate::pipeline(&p).unwrap().0;
    //     let deps = DependencyEnv::new().scan(&p);
    //     let xvar = named(0, "x");
    //     let yvar = named(2, "y");
    //     let tvar = named(4, "t");
    //     let fvar = named(5, "f");
    //     assert_root!(deps: xvar, yvar);
    //     assert_family!(deps: tvar => xvar, yvar);
    //     assert_family!(deps: fvar => tvar);
    //     assert_eq!(deps.len(), 4);
    // }

    // #[test]
    // pub fn test_hypergraphs_works_as_expected_for_samples() {
    //     let p = program!(lets![
    //        "x" ;= flip!(1/3);
    //        "s" ;= sample!(var!("x"));
    //        ...? b!("s")
    //     ]);
    //     let p = annotate::pipeline(&p).unwrap().0;
    //     let deps = DependencyEnv::new().scan(&p);
    //     let xvar = named(0, "x");
    //     let svar = named(2, "s");
    //     assert_root!(deps: xvar);
    //     assert_family!(deps: svar => xvar);
    //     assert_eq!(deps.len(), 2);
    // }

    // #[test]
    // pub fn test_hypergraphs_ite_sample() {
    //     let p = program!(lets![
    //         "x" ;= flip!(1/5);
    //         "y" ;= flip!(1/5);
    //         "z" ;= ite!(
    //             if ( var!("x") )
    //             then { sample!(flip!(1/3)) }
    //             else { var!("y") });
    //         ...? b!("y")
    //     ]);
    //     let p = annotate::pipeline(&p).unwrap().0;
    //     let deps = DependencyEnv::new().scan(&p);
    //     let xvar = named(0, "x");
    //     let yvar = named(2, "y");
    //     let zvar = named(4, "z");
    //     assert_root!(deps: xvar, yvar);
    //     assert_family!(deps: zvar => xvar, yvar);
    //     assert_eq!(deps.len(), 3);
    // }
}
