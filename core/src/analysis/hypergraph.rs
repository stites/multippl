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

impl<V> Edge<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    pub fn from(i: &[V]) -> Self {
        Edge(i.iter().cloned().collect())
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
    pub fn empty() -> Self {
        Self {
            cover: Default::default(),
            edges: Default::default(),
        }
    }
    pub fn from_edge(e: &Edge<V>) -> Self {
        let vs: &HashSet<V> = &e.0;
        Self {
            cover: e.0.clone(),
            edges: HashSet::from([e.clone()]),
        }
    }
    pub fn merge(cs: HashSet<Cover<V>>) -> Self {
        let span = tracing::span!(tracing::Level::DEBUG, "Cover::merge");
        let _guard = span.enter();
        let ret = cs.into_iter().fold(Self::empty(), |mut ret, cover| {
            debug!("-> {:?} + {:?}", ret, cover);
            ret.cover.extend(cover.cover);
            ret.edges.extend(cover.edges);
            ret
        });
        debug!("==> {:?}", ret);
        ret
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AllCovers<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    covers: HashSet<Cover<V>>,
}
impl<V> AllCovers<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    pub fn size(&self) -> usize {
        self.covers.len()
    }

    pub fn new(covers: HashSet<Cover<V>>) -> Self {
        AllCovers { covers }
    }
    pub fn from_edges(es: impl Iterator<Item = Edge<V>>) -> Self {
        let span = tracing::span!(tracing::Level::DEBUG, "AllClusters::from_edges");
        let _guard = span.enter();

        let empty = AllCovers {
            covers: Default::default(),
        };
        let seen: HashSet<V> = HashSet::new();
        es.fold((empty, seen), |(mut all, mut seen), edge| {
            let e = Cover::from_edge(&edge);
            if seen.is_disjoint(&e.cover) {
                debug!("new cover with: {:?}", edge);

                seen.extend(e.cover.clone());
                all.covers.insert(e);
                (all, seen)
            } else {
                debug!("merging into cover: {:?}", edge);
                debug!("all.covers: {:?}", all.covers);

                let (mut to_merge, mut rest): (HashSet<Cover<V>>, HashSet<Cover<V>>) =
                    all.covers.into_iter().partition_map(|cover| {
                        if cover.cover.is_disjoint(&e.cover) {
                            Either::Right(cover)
                        } else {
                            Either::Left(cover)
                        }
                    });
                seen.extend(e.cover.clone());
                to_merge.insert(e);
                debug!("covers to merge:");
                for c in &to_merge {
                    debug!("- {:?}", c);
                }
                debug!("covers to ignore:");
                for c in &rest {
                    debug!("- {:?}", c);
                }
                rest.insert(Cover::merge(to_merge));
                (AllCovers::new(rest), seen)
            }
        })
        .0
    }
    pub fn remove_edge(&mut self, e: &Edge<V>) {
        let span = tracing::span!(tracing::Level::DEBUG, "AllClusters::remove_edge");
        let _guard = span.enter();
        debug!("removing {:?} from {:?}", e, self);

        let mut to_split = self
            .covers
            .iter()
            .find(|c| c.edges.contains(e))
            .expect("edge not found in covers")
            .clone();
        debug!("found cover {:?}", to_split);
        self.covers.remove(&to_split);
        to_split.edges.remove(e);
        debug!("pruned edges: {:?}", to_split.edges);
        let new_covers = Self::from_edges(to_split.edges.into_iter());
        debug!("new covers: {:?}", new_covers);
        self.covers.extend(new_covers.covers);
        debug!("final set: {:?}", self);
    }
}

pub trait Hypergraph
where
    <Self as Hypergraph>::Vertex: Clone + Debug + PartialEq + Eq + Hash,
{
    type Vertex;
    fn vertices(&self) -> &HashSet<Self::Vertex>;
    fn hyperedges(&self) -> std::vec::IntoIter<Edge<Self::Vertex>>;
    fn insert_edge(&mut self, edge: Edge<Self::Vertex>) -> bool;
    fn insert_vertex(&mut self, v: Self::Vertex) -> bool;

    /// cut a vertex out of the hypergraph
    fn edgecuts_ranked(&self) -> Vec<(Edge<Self::Vertex>, Rank)> {
        let all_covers = self.covers();
        self.hyperedges()
            .map(|edge| {
                let mut simulation = all_covers.clone();
                simulation.remove_edge(&edge);
                (edge.clone(), Rank(simulation.size()))
            })
            .collect()
    }
    fn edgecuts_sorted(&self) -> Vec<(Edge<Self::Vertex>, Rank)> {
        let mut sorted_edges = self.edgecuts_ranked();
        sorted_edges.sort_by(|(_, a), (_, b)| b.cmp(&a));
        sorted_edges
    }
    fn covers(&self) -> AllCovers<Self::Vertex> {
        AllCovers::from_edges(self.hyperedges())
    }
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
    fn insert_edge(&mut self, edge: Edge<V>) -> bool {
        debug!("insert edge: {:?}", edge);
        self.vertices.extend(edge.0.clone());
        self.hyperedges.insert(edge.clone())
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
#[cfg(test)]
#[allow(unused_must_use)]
mod hgraph_test {
    use super::*;

    #[test]
    fn test_cover_creation() {
        crate::utils::enable_traced_test();
        let mut g: HGraph<usize> = Default::default();
        g.insert_vertex(0);
        g.insert_vertex(1);
        g.insert_vertex(2);
        g.insert_vertex(7);
        g.insert_vertex(8);
        g.insert_vertex(9);
        g.insert_edge(Edge::from(&[0, 2]));
        assert_eq!(g.covers().size(), 1);
        g.insert_edge(Edge::from(&[1, 2]));
        assert_eq!(g.covers().size(), 1);
        g.insert_edge(Edge::from(&[0, 1, 2]));
        let cs = g.covers();
        assert_eq!(cs.size(), 1);
        let c = cs.covers.into_iter().nth(0).unwrap();
        assert_eq!(c.edges.len(), 3);

        g.insert_edge(Edge::from(&[7, 8, 9]));
        assert_eq!(g.covers().covers.len(), 2);
    }

    #[test]
    fn test_cover_edge_removal() {
        crate::utils::enable_traced_test();
        let mut g: HGraph<usize> = Default::default();
        g.insert_vertex(0);
        g.insert_vertex(1);
        g.insert_vertex(2);
        let e0 = Edge::from(&[0]);
        let e1 = Edge::from(&[1, 2]);
        let e2 = Edge::from(&[0, 1, 2]);
        g.insert_edge(e0.clone());
        g.insert_edge(e1.clone());
        g.insert_edge(e2.clone());
        let mut cs = g.covers();
        assert_eq!(cs.size(), 1);
        cs.remove_edge(&e2);
        assert_eq!(cs.size(), 2);

        g.insert_vertex(7);
        g.insert_vertex(8);
        g.insert_vertex(9);
        let e3 = Edge::from(&[7, 8, 9]);
        g.insert_edge(e3.clone());
        let mut cs = g.covers();
        cs.remove_edge(&e3);
        assert_eq!(cs.size(), 1);
    }
}
#[derive(Clone, Debug)]
pub struct ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    graph: HGraph<Cluster<V>>,
    intersections_inv: HashMap<Edge<Cluster<V>>, HashSet<V>>,
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
            intersections_inv: Default::default(),
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
    fn insert_edge(&mut self, edge: Edge<Self::Vertex>) -> bool {
        self.graph.insert_edge(edge)
    }

    /// add a vertex to the hypergraph. Returns false if the vertex is already in the hypergraph
    fn insert_vertex(&mut self, v: Self::Vertex) -> bool {
        self.graph.insert_vertex(v)
    }
}
#[derive(Clone, Copy, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rank(pub usize);

impl<V> ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    pub fn variable_intersection(edge: &Edge<Cluster<V>>) -> V
    where
        V: Debug + PartialEq + Clone + Eq + Hash,
    {
        let common = Self::common_variables(&edge.0);
        debug!("{:?} common variables: {:?}", edge, common);
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
        let ret: HashMap<Edge<Cluster<V>>, HashSet<V>> = self
            .hyperedges()
            .map(|edge| (edge.clone(), Self::common_variables(&edge.0)))
            .collect();
        self.intersections_inv = ret.clone();
        self.intersections = ret
            .clone()
            .into_iter()
            .fold(HashMap::new(), |mut inv, (e, vs)| {
                for v in vs {
                    match inv.get_mut(&v) {
                        None => {
                            inv.insert(v.clone(), HashSet::from([e.clone()]));
                        }
                        Some(es) => {
                            es.insert(e.clone());
                        }
                    }
                }
                inv
            });
    }

    /// cut a vertex out of the hypergraph
    fn edgecuts_ranked(&self) -> Vec<(V, Rank)> {
        let check : HashMap<V, Vec<Rank>> = <Self as Hypergraph>::edgecuts_ranked(self).into_iter().fold(HashMap::new(),
            |mut ranks, (e, r)| {
                match self.intersections_inv.get(&e) {
                    None => panic!("expected all edges intersection map, perhaps you need to rebuild intersections"),
                        Some(vs) => {
                            for v in vs {
                                match ranks.get_mut(&v) {
                                    None => {ranks.insert(v.clone(), vec![r.clone()]); },
                                    Some(rs) => {rs.push(r.clone());},
                                }
                            }
                        }
                }
                    ranks
            }

        );
        check.into_iter().fold(vec![], |mut ret, (v, rs)| {
            assert!(rs.iter().all(|r| r == &rs[0]));
            ret.push((v, rs[0]));
            ret
        })
    }
    fn edgecuts_sorted(&self) -> Vec<(V, Rank)> {
        let mut sorted_edges = self.edgecuts_ranked();
        sorted_edges.sort_by(|(_, a), (_, b)| b.cmp(&a));
        sorted_edges
    }
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
        g.insert_edge(Edge(edge.clone()));
    }
    g.rebuild_intersections();
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

#[cfg(test)]
#[allow(unused_mut)]
#[allow(unused_must_use)]
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
        ($prog:expr; $($name:ident: $test:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let _ = crate::utils::enable_traced_test();
                let g : ClusterGraph<NamedVar> = pipeline(&$prog);
                ($test)(g);
            }
        )*
        }
    }

    tests_for_program! {
        program!(lets![
            "x" ;= flip!(1/3);
           ...? b!("x")
        ]);
        test_hypergraphs_for_simple_program: |g: G| {
            let xvar = named(0, "x");
            assert_clusters!(g, vars: &[&named(0, "x")]);
            assert_eq!(g.vertices().len(), 1);
            debug!("{}", g.graph.print());
            assert_edges!(g { xvar => [[&xvar]] } );
        },
        test_cuts_for_simple_program: |g: G| {
            let ecs: Vec<(NamedVar, Rank)> = g.edgecuts_sorted();
            assert_eq!(ecs.len(), 1);
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
        test_hypergraphs_with_boolean_operator: |g: G| {
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
        // test_cuts_with_boolean_operator: |g: G| {
        //     let ecs: Vec<(NamedVar, Rank)> = g.edgecuts_sorted();
        //     assert_eq!(ecs.len(), 5);
        //     println!("{:?}", ecs);
        //     todo!()
        // },
    }
    tests_for_program! {
        program!(lets![
            "x" ;= flip!(1/3);
            "y" ;= flip!(1/3);
            "t" ;= b!("x", "y");
            "f" ;= fst!("t");
           ...? b!("t")
        ]);
        test_hypergraphs_captures_tuples: |g: G| {
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
        // test_cuts_with_boolean_operator: |g: G| {
        //     let ecs: Vec<(NamedVar, Rank)> = g.edgecuts_sorted();
        //     assert_eq!(ecs.len(), 5);
        //     println!("{:?}", ecs);
        //     todo!()
        // },
    }

    tests_for_program! {
        program!(lets![
           "x" ;= flip!(1/3);
           "s" ;= sample!(var!("x"));
           ...? b!("s")
        ]);
        test_hypergraphs_treats_sample_statements_as_cuts: |g: G| {
            let xvar = named(0, "x");
            let svar = named(2, "s");

            assert_clusters!(g, vars: &[&xvar], &[&svar]);
            assert_edges!(g {
                xvar => [[&xvar]],
                svar => [[&svar]]
            });
        },
        // test_cuts_with_boolean_operator: |g: G| {
        //     let ecs: Vec<(NamedVar, Rank)> = g.edgecuts_sorted();
        //     assert_eq!(ecs.len(), 5);
        //     println!("{:?}", ecs);
        //     todo!()
        // },
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
        test_hypergraphs_ite_sample: |g: G| {
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
        // test_cuts_with_boolean_operator: |g: G| {
        //     let ecs: Vec<(NamedVar, Rank)> = g.edgecuts_sorted();
        //     assert_eq!(ecs.len(), 5);
        //     println!("{:?}", ecs);
        //     todo!()
        // },
    }
}
