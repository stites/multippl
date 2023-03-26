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
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use tracing::*;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

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
        let mut hashes = self.0.iter().map(calculate_hash).collect_vec();
        hashes.sort();
        let hashstr = hashes.into_iter().map(|x| x.to_string()).join("");
        hashstr.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cover<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    cover: HashSet<V>,
    edges: HashSet<Edge<V>>,
}
impl<V> Hash for Cover<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        let mut hashes = vec![];
        let cover_hashes = self.cover.iter().map(calculate_hash).collect_vec();
        hashes.extend(cover_hashes);
        let edge_hashes = self.edges.iter().map(calculate_hash).collect_vec();
        hashes.extend(edge_hashes);
        hashes.sort();
        let hashstr = hashes.into_iter().map(|x| x.to_string()).join("");
        hashstr.hash(state);
    }
}

impl<V> Cover<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    pub fn from_edge(e: &Edge<V>) -> Self {
        let vs: &HashSet<V> = &e.0;
        Self {
            cover: e.0.clone(),
            edges: HashSet::from([e.clone()]),
        }
    }
    pub fn from_edges(es: &HashSet<Edge<V>>) -> HashSet<Self> {
        es.iter()
            .map(Self::from_edge)
            .fold(HashSet::new(), |covers, edge_cover| {
                todo!()

                // fn edges_to_covers(hyperedges: impl IntoIterator<Item = Edge<V>>) -> Vec<Cover<V>> {
                //     let mut overlaps: Vec<Cover<V>> = vec![];
                //     for edge in hyperedges {
                //         let var = variable_intersection(&edge);
                //         let overlaps_for_edge: Vec<Edge<V>> = overlaps
                //             .iter()
                //             .filter(|(_, oedge)| !oedge.0.is_disjoint(edge.0))
                //             .cloned()
                //             .collect_vec();

                //         // if overlaps_for_edge.is_empty() {
                //         //     overlaps.push((edge.clone(), vec![(edge, label)]))
                //         // } else {
                //         //     // update a cover
                //         //     let mut new_cover = edge.clone();
                //         //     let mut new_cover_edges = vec![(edge, label)];
                //         //     let mut diff = 0;
                //         //     for (cover_ix, cover, edges) in overlaps_for_edge {
                //         //         new_cover.extend(cover.clone());
                //         //         new_cover_edges.extend(edges);
                //         //         overlaps.remove(cover_ix - diff);
                //         //         diff += 1;
                //         //     }
                //         //     let nonempty_new_cover_edges = new_cover_edges
                //         //         .into_iter()
                //         //         .filter(|s| !s.0.is_empty())
                //         //         .collect();
                //         //     overlaps.push((
                //         //         new_cover,
                //         //         dedupe_labeled_hashset_refs(nonempty_new_cover_edges),
                //         //     ));
                //         // }
                //     }
                //     overlaps
                // }
            })
    }
}

pub trait Hypergraph
where
    <Self as Hypergraph>::Vertex: Clone + Debug + PartialEq + Eq + Hash,
{
    type Vertex;
    fn vertices(&self) -> &HashSet<Self::Vertex>;
    fn hyperedges(&self) -> std::vec::IntoIter<Edge<Self::Vertex>>;
    fn insert_edge(&mut self, edge: &HashSet<Self::Vertex>) -> bool;
    fn insert_vertex(&mut self, v: Self::Vertex) -> bool;

    /// cut a vertex out of the hypergraph
    fn rank_edge_cuts(&self) -> Vec<(Edge<Self::Vertex>, Rank)> {
        self.hyperedges()
            .map(|edge| (edge.clone(), self.edge_cut_rank(&edge)))
            .collect()
    }

    fn edge_cut_rank(&self, edge: &Edge<Self::Vertex>) -> Rank {
        let nextedges: HashSet<Edge<Self::Vertex>> =
            self.hyperedges().filter(|e| e != edge).collect();
        Rank(Cover::from_edges(&nextedges).len())
    }
    // pub fn covers(&self) -> Vec<(HashSet<V>, Vec<(&HashSet<V>, &EL)>)> {
    //     Self::edges_to_covers(&self.hyperedges)
    // }
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

    fn vertices(&self) -> &HashSet<V> {
        &self.vertices
    }

    fn hyperedges<'a>(&self) -> std::vec::IntoIter<Edge<Self::Vertex>> {
        self.hyperedges
            .iter()
            .map(|x| Edge(x.0.clone()))
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
}
#[derive(Clone, Debug)]
pub struct ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    graph: HGraph<Cluster<V>>,
    intersections: HashMap<V, HashSet<Edge<Cluster<V>>>>,
}
impl<V> Default for ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    fn default() -> Self {
        Self {
            graph: Default::default(),
            intersections: Default::default(),
        }
    }
}
impl<V> Hypergraph for ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    type Vertex = Cluster<V>;
    fn vertices(&self) -> &HashSet<Self::Vertex> {
        self.graph.vertices()
    }

    fn hyperedges<'a>(&self) -> std::vec::IntoIter<Edge<Self::Vertex>> {
        self.graph.hyperedges()
    }

    /// add an edge to the hypergraph. Returns false if the edge is already in the hypergraph
    fn insert_edge(&mut self, edge: &HashSet<Self::Vertex>) -> bool {
        self.graph.insert_edge(edge)
    }

    /// add a vertex to the hypergraph. Returns false if the vertex is already in the hypergraph
    fn insert_vertex(&mut self, v: Self::Vertex) -> bool {
        self.graph.insert_vertex(v)
    }
}

impl<V> ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    pub fn variable_intersection(edge: &Edge<Cluster<V>>) -> V
    where
        V: Debug + PartialEq + Clone + Eq + Hash,
    {
        let common = Self::common_variables(&edge.0);
        assert_eq!(common.len(), 1);
        common.iter().nth(0).unwrap().clone()
    }

    pub fn common_variables(clusters: &HashSet<Cluster<V>>) -> HashSet<V>
    where
        V: Debug + PartialEq + Clone + Eq + Hash,
    {
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
    pub fn rebuild_intersections(&mut self) {
        let mut ret: HashMap<V, HashSet<Edge<Cluster<V>>>> = HashMap::new();
        for edge in self.hyperedges() {
            for cluster in edge.0.iter() {
                for name in cluster.0.iter() {
                    match ret.get_mut(&name) {
                        None => {
                            ret.insert(name.clone(), HashSet::from([edge.clone()]));
                        }
                        Some(es) => {
                            es.insert(edge.clone());
                        }
                    }
                }
            }
        }
        self.intersections = ret;
    }
}

#[derive(Clone, Copy, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rank(pub usize);

impl HGraph<Cluster<NamedVar>> {}

pub fn order_cuts(g: &ClusterGraph<NamedVar>) -> Vec<(NamedVar, Rank)> {
    let mut ranking = vec![];
    let mut sorted_edges = g.rank_edge_cuts();
    sorted_edges.sort_by(|(_, a), (_, b)| b.cmp(&a));
    for (edge, rank) in sorted_edges {
        let var = ClusterGraph::variable_intersection(&edge);
        ranking.push((var, rank));
    }
    ranking
}
pub fn top_k_cuts(cuts: &Vec<(NamedVar, Rank)>, n: usize) -> Vec<NamedVar> {
    assert!(
        n - 1 < cuts.len(),
        "requested {} cuts, but only {} possible cut{} found",
        n,
        cuts.len(),
        String::from(if cuts.len() == 1 { "" } else { "s" })
    );
    cuts.iter().map(|(b, r)| b.clone()).take(n).collect()
}

pub fn build_graph(deps: &DependenceMap) -> ClusterGraph<NamedVar> {
    let mut g = ClusterGraph::default();
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
        g.insert_edge(edge);
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
        let mut hashes = self.0.iter().map(calculate_hash).collect_vec();
        hashes.sort();
        let hashstr = hashes.into_iter().map(|x| x.to_string()).join("");
        hashstr.hash(state);
    }
}

pub fn pipeline(p: &crate::ProgramInferable) -> ClusterGraph<NamedVar> {
    let p = annotate::pipeline(&p).unwrap().0;
    let deps = DependencyEnv::new().scan(&p);
    build_graph(&deps)
}

#[allow(unused_mut)]
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

    fn named_to_cluster(xs: &[&NamedVar]) -> Cluster<NamedVar> {
        Cluster(xs.iter().map(Clone::clone).cloned().collect())
    }

    macro_rules! assert_clusters {
        ($g:ident : $cvar:expr) => {{
            let span = tracing::span!(tracing::Level::DEBUG, "assert_clusters");
            let _guard = span.enter();
            let cs = $g.vertices();
            debug!("{:?}#{} in the following?", $cvar, calculate_hash(&$cvar));
            for c in cs {
                debug!("vertex: {:?}#{}", c, calculate_hash(&c));
            }
            assert!(cs.contains(&$cvar), "expected {:?} in hypergraph. found: {:?}", $cvar, cs);
        }};
        ($g:ident : $xvar:expr $(, $var:expr)+) => {{
            assert_clusters!($g : $xvar);
            $(
            assert_clusters!($g : $var);
            )+
        }};
        ($g:ident, vars: $xvar:expr) => {{
            assert_clusters!($g : named_to_cluster($xvar));
        }};
        ($g:ident, vars : $xvar:expr $(, $var:expr)+) => {{
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
            let n2e = $g.names_to_edges();
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

    #[test]
    pub fn test_hypergraphs_for_simple_program() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
           ...? b!("x")
        ]);
        let g = pipeline(&p);

        let xvar = named(0, "x");
        assert_clusters!(g, vars: &[&named(0, "x")]);
        assert_eq!(g.vertices().len(), 1);
        debug!("{}", g.print());

        assert_edges!(g { xvar => [[&xvar]] } );
    }

    #[test]
    pub fn test_hypergraphs_with_boolean_operator() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "z" ;= b!("x" && "y");
            "q" ;= flip!(1/3);
            "w" ;= b!("q" && "z");
           ...? b!("z")
        ]);
        let g = pipeline(&p);
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
    }

    #[test]
    pub fn test_hypergraphs_captures_tuples() {
        let p = program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "t" ;= b!("x", "y");
            "f" ;= fst!("t");
           ...? b!("t")
        ]);
        let g = pipeline(&p);
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
    }

    #[test]
    pub fn test_hypergraphs_treats_sample_statements_as_cuts() {
        // crate::utils::enable_traced_test();
        let p = program!(lets![
           "x" ;= flip!(1/3);
           "s" ;= sample!(var!("x"));
           ...? b!("s")
        ]);
        let g = pipeline(&p);
        let xvar = named(0, "x");
        let svar = named(2, "s");

        assert_clusters!(g, vars: &[&xvar], &[&svar]);
        assert_edges!(g {
            xvar => [[&xvar]],
            svar => [[&svar]]
        });
    }

    #[test]
    pub fn test_hypergraphs_ite_sample() {
        let p = program!(lets![
            "x" ;= flip!(1/5);
            "y" ;= flip!(1/5);
            "z" ;= flip!(1/5);
            "i" ;= ite!(
                if ( var!("x") )
                then { sample!(var!("y")) }
                else { var!("z") });
            ...? b!("y")
        ]);
        let g = pipeline(&p);
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
    }
}
