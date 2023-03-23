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
use core::hash::Hash;
use std::collections::{HashMap, HashSet};
use tracing::*;

pub type IteractionGraph = Hypergraph<PlanPtr, (), Binding>;
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PlanPtr(pub usize);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlanNode {
    And(PlanPtr, PlanPtr),
    Or(PlanPtr, PlanPtr),
    Iff(PlanPtr, PlanPtr),
    Ite(PlanPtr, PlanPtr, PlanPtr),
    Not(PlanPtr),
    ConstTrue,
    ConstFalse,
    Literal(VarLabel, bool),
}
pub struct PlanManager {
    gensym: usize,
    store: HashMap<PlanPtr, PlanNode>,
}
impl Default for PlanManager {
    fn default() -> Self {
        PlanManager {
            gensym: 0,
            store: Default::default(),
        }
    }
}
impl PlanManager {
    fn fresh(&mut self) -> PlanPtr {
        let p = PlanPtr(self.gensym);
        self.gensym += 1;
        p
    }
    pub fn flatten(&self, ptr: PlanPtr) -> HashSet<PlanPtr> {
        use PlanNode::*;
        match self.store.get(&ptr).unwrap() {
            ConstTrue | ConstFalse => HashSet::new(),
            Literal(_, _) => HashSet::from([ptr]),
            Not(p) => self.flatten(*p),
            Iff(l, r) => self.flatten_all(&[*l, *r]),
            Or(l, r) => self.flatten_all(&[*l, *r]),
            And(l, r) => self.flatten_all(&[*l, *r]),
            Ite(p, l, r) => self.flatten_all(&[*p, *l, *r]),
        }
    }
    pub fn flatten_all(&self, ps: &[PlanPtr]) -> HashSet<PlanPtr> {
        ps.into_iter().fold(HashSet::new(), |mut fin, p| {
            fin.extend(&self.flatten(*p));
            fin
        })
    }
    pub fn var(&mut self, v: VarLabel, polarity: bool) -> PlanPtr {
        let lit = PlanNode::Literal(v, polarity);
        let ptr = self.fresh();
        self.store.insert(ptr, lit);
        ptr
    }
    fn binop_h(
        &mut self,
        l: PlanPtr,
        r: PlanPtr,
        op: &dyn Fn(PlanPtr, PlanPtr) -> PlanNode,
    ) -> PlanPtr {
        let ptr = self.fresh();
        let bdd = op(l, r);
        self.store.insert(ptr, bdd);
        ptr
    }
    pub fn and(&mut self, l: PlanPtr, r: PlanPtr) -> PlanPtr {
        self.binop_h(l, r, &|l, r| PlanNode::And(l, r))
    }
    pub fn or(&mut self, l: PlanPtr, r: PlanPtr) -> PlanPtr {
        self.binop_h(l, r, &|l, r| PlanNode::Or(l, r))
    }
    pub fn iff(&mut self, l: PlanPtr, r: PlanPtr) -> PlanPtr {
        self.binop_h(l, r, &|l, r| PlanNode::Iff(l, r))
    }
    pub fn ite(&mut self, p: PlanPtr, l: PlanPtr, r: PlanPtr) -> PlanPtr {
        let ptr = self.fresh();
        let bdd = PlanNode::Ite(p, l, r);
        self.store.insert(ptr, bdd);
        ptr
    }
    pub fn not(&mut self, p: PlanPtr) -> PlanPtr {
        let ptr = self.fresh();
        let bdd = PlanNode::Not(p);
        self.store.insert(ptr, bdd);
        ptr
    }
    pub fn bool(&mut self, p: bool) -> PlanPtr {
        let ptr = self.fresh();
        let bdd = if p {
            PlanNode::ConstTrue
        } else {
            PlanNode::ConstFalse
        };
        self.store.insert(ptr, bdd);
        ptr
    }
}

#[derive(Clone, Debug)]
pub struct Hypergraph<V, VL, EL>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
    VL: Clone + Debug + PartialEq + Eq + Hash,
    EL: Clone + Debug + PartialEq + Eq + Hash,
{
    pub vertices: HashSet<(V, VL)>,
    pub hyperedges: Vec<(HashSet<V>, EL)>,
}
impl<V, VL, EL> Default for Hypergraph<V, VL, EL>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
    VL: Clone + Debug + PartialEq + Eq + Hash,
    EL: Clone + Debug + PartialEq + Eq + Hash,
{
    fn default() -> Self {
        Hypergraph {
            vertices: Default::default(),
            hyperedges: Default::default(),
        }
    }
}
fn dedupe_labeled_hashset_refs<'a, T: Hash + Eq, L: Hash + Eq + Clone>(
    hss: Vec<(&'a HashSet<T>, &'a L)>,
) -> Vec<(&'a HashSet<T>, &'a L)> {
    let mut cache: HashMap<(L, usize), Vec<(&HashSet<T>, &L)>> = HashMap::new();
    for (cur, lbl) in hss {
        let len = cur.len(); // an incredibly stupid hashset for the time being
        match cache.get_mut(&(lbl.clone(), len)) {
            None => {
                cache.insert((lbl.clone(), len), vec![(cur, lbl)]);
            }
            Some(cached_hss) => {
                let mut add = true;
                for (cached_hs, cached_label) in cached_hss.iter() {
                    if cached_label == &lbl || cached_hs.symmetric_difference(cur).next().is_none()
                    {
                        add = false;
                        break;
                    }
                }
                if add {
                    cached_hss.push((cur, lbl));
                }
            }
        }
    }
    cache.into_values().flatten().collect()
}

impl<V, EL> Hypergraph<V, (), EL>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
    EL: Clone + Debug + PartialEq + Eq + Hash,
{
    /// add an edge to the hypergraph. Returns false if the edge is already in the hypergraph
    pub fn insert_edge(&mut self, edge: &HashSet<V>, l: EL) -> bool {
        debug!("insert edge: {:?} @ {:?}", edge, l);

        let verts: HashSet<&V> = self.vertices.iter().map(|x| &x.0).collect();
        let edgerefs: HashSet<&V> = edge.iter().collect();
        let new_verts: HashSet<(V, ())> = edgerefs
            .difference(&verts)
            .cloned()
            .map(|x| (x.clone(), ()))
            .collect();
        if !new_verts.is_empty() {
            self.vertices.extend(new_verts);
        }

        let _next_ix = self.hyperedges.len();
        match self.hyperedges.iter().find(|e| e.0 == *edge && e.1 == l) {
            Some(_) => return false,
            None => self.hyperedges.push((edge.clone(), l)),
        }
        true
    }
}
impl<V, VL, EL> Hypergraph<V, VL, EL>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
    VL: Clone + Debug + PartialEq + Eq + Hash,
    EL: Clone + Debug + PartialEq + Eq + Hash,
{
    /// add a vertex to the hypergraph. Returns false if the vertex is already in the hypergraph
    pub fn insert_vertex(&mut self, v: V, l: VL) -> bool {
        self.vertices.insert((v, l))
    }

    pub fn print(&self) -> String {
        let vtxs = self
            .vertices
            .iter()
            .map(|x| format!("{:?}", x.0))
            .join(", ");
        let edges = self
            .hyperedges
            .iter()
            .map(|x| format!("{:?}", x.0))
            .join(",\n");
        let mut s = String::from("");
        s.push_str(&format!("vertices: {{ {} }}\n", vtxs));
        s.push_str("edges:\n");
        for (edge, name) in &self.hyperedges {
            s.push_str(&format!("  | {:?} -@-> {:?}\n", edge, name));
        }
        s
    }

    /// cut a vertex out of the hypergraph
    pub fn rank_edge_cuts(&self) -> Vec<(&HashSet<V>, &EL, Rank)> {
        self.hyperedges
            .iter()
            .map(|(edge, l)| (edge, l, self.edge_cut_rank(edge, l)))
            .collect()
    }

    pub fn edge_cut_rank(&self, edge: &HashSet<V>, label: &EL) -> Rank {
        let nextedges = self
            .hyperedges
            .iter()
            .filter(|(e, l)| l != label && e != edge)
            .cloned()
            .collect_vec();
        let newcovers = Self::edges_to_covers(&nextedges);
        Rank(newcovers.len())
    }

    pub fn edges_to_covers(
        hyperedges: &Vec<(HashSet<V>, EL)>,
    ) -> Vec<(HashSet<V>, Vec<(&HashSet<V>, &EL)>)> {
        let mut overlaps: Vec<(HashSet<V>, Vec<(&HashSet<V>, &EL)>)> = vec![];
        for (edge, label) in hyperedges.iter() {
            let overlaps_for_edge: Vec<(usize, HashSet<V>, Vec<(&HashSet<V>, &EL)>)> = overlaps
                .iter()
                .enumerate()
                .filter(|(_, (oedge, _))| !oedge.is_disjoint(edge))
                .map(|(a, (b, c))| (a, b.clone(), c.clone()))
                .collect_vec();

            if overlaps_for_edge.is_empty() {
                overlaps.push((edge.clone(), vec![(edge, label)]))
            } else {
                // update a cover
                let mut new_cover = edge.clone();
                let mut new_cover_edges = vec![(edge, label)];
                let mut diff = 0;
                for (cover_ix, cover, edges) in overlaps_for_edge {
                    new_cover.extend(cover.clone());
                    new_cover_edges.extend(edges);
                    overlaps.remove(cover_ix - diff);
                    diff += 1;
                }
                let nonempty_new_cover_edges = new_cover_edges
                    .into_iter()
                    .filter(|s| !s.0.is_empty())
                    .collect();
                overlaps.push((
                    new_cover,
                    dedupe_labeled_hashset_refs(nonempty_new_cover_edges),
                ));
            }
        }
        overlaps
    }

    pub fn covers(&self) -> Vec<(HashSet<V>, Vec<(&HashSet<V>, &EL)>)> {
        Self::edges_to_covers(&self.hyperedges)
    }
}

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub enum Binding {
    Unbound,
    Alias,
    Let(NamedVar),
    NamedSample(NamedVar),
    NamedSampleLet(NamedVar, NamedVar),
    Query,
    CompoundQuery(usize),
    // TODO: beyond the prototype, these would be warranted:
    // InUnnamedSample,
    // InUnnamedIteBranch(bool),
    // NamedIteBranch(bool, NamedVar),
}
impl Binding {
    pub fn id(&self) -> Option<UniqueId> {
        use Binding::*;
        match self {
            Alias | Unbound | Query | CompoundQuery(_) => None,
            Let(v) | NamedSample(v) | NamedSampleLet(_, v) => Some(v.id()),
        }
    }
    pub fn sample_id(&self) -> Option<UniqueId> {
        use Binding::*;
        match self {
            Alias | Unbound | Query | CompoundQuery(_) | Let(_) => None,
            NamedSample(v) | NamedSampleLet(v, _) => Some(v.id()),
        }
    }
    pub fn cut_id(&self) -> Option<UniqueId> {
        use Binding::*;
        match self {
            Alias | Unbound | Query | CompoundQuery(_) | NamedSample(_) | NamedSampleLet(_, _) => {
                None
            }
            Let(v) => Some(v.id()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MaxUniqueId(pub u64);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MaxVarLabel(pub u64);

pub struct InteractionEnv {
    graph: Hypergraph<PlanPtr, (), Binding>,
    mgr: PlanManager,
    binding: Binding,
    mx_uid: MaxUniqueId,
    mx_lbl: MaxVarLabel,
}

struct Ctx {
    substitutions: HashMap<NamedVar, Vec<PlanPtr>>,
}
impl Default for Ctx {
    fn default() -> Self {
        Self {
            substitutions: Default::default(),
        }
    }
}

impl Ctx {
    fn insert_sub(&mut self, v: NamedVar, d: Vec<PlanPtr>) {
        self.substitutions.insert(v, d);
    }
}

impl Default for InteractionEnv {
    fn default() -> InteractionEnv {
        Self {
            mgr: Default::default(),
            graph: Default::default(),
            binding: Binding::Unbound,
            mx_uid: MaxUniqueId(0),
            mx_lbl: MaxVarLabel(0),
        }
    }
}
impl InteractionEnv {
    fn plan_anf(&mut self, ctx: &Ctx, a: &AnfAnn) -> Result<Vec<PlanPtr>, CompileError> {
        use crate::grammar::Anf::*;
        match a {
            AVar(v, s) => {
                if v.id().0 > self.mx_uid.0 {
                    self.mx_uid = MaxUniqueId(v.id().0);
                }
                match ctx.substitutions.get(&v) {
                    None => panic!("must already be included"),
                    Some(ptrs) => Ok(ptrs.clone()),
                }
            }
            AVal(_, Val::Bool(b)) => Ok(vec![self.mgr.bool(*b)]),
            AVal(_, Val::Prod(_)) => todo!(),
            And(bl, br) => {
                let b = match &self.binding {
                    Binding::Query => Binding::CompoundQuery(0),
                    a => a.clone(),
                };

                self.binding = b;

                let pls = self.plan_anf(ctx, bl)?;
                let prs = self.plan_anf(ctx, br)?;
                let ret = izip!(pls, prs)
                    .map(|(l, r)| self.mgr.and(l, r))
                    .collect_vec();
                Ok(ret)
            }
            Or(bl, br) => {
                let b = match &self.binding {
                    Binding::Query => Binding::CompoundQuery(0), // 0 is just a placeholder for now
                    a => a.clone(),
                };
                self.binding = b;

                let pls = self.plan_anf(ctx, bl)?;
                let prs = self.plan_anf(ctx, br)?;
                let ret = izip!(pls, prs)
                    .map(|(l, r)| self.mgr.or(l, r))
                    .collect_vec();
                Ok(ret)
            }
            Neg(bl) => {
                let prs = self.plan_anf(ctx, bl)?;
                Ok(prs.iter().map(|p| self.mgr.not(*p)).collect_vec())
            }
        }
    }
    fn plan_anfs(&mut self, ctx: &Ctx, anfs: &[AnfAnn]) -> Result<Vec<PlanPtr>, CompileError> {
        anfs.iter()
            .map(|a| {
                let res = self.plan_anf(ctx, a)?;
                assert_eq!(res.len(), 1);
                // let deps = self.mgr.flatten(res[0]);
                debug!("print: {:?}", a);
                // self.graph.insert_edge(&deps, self.binding.clone());
                Ok(res[0])
            })
            .collect()
    }
    fn plan_expr(&mut self, ctx: &mut Ctx, e: &ExprAnn) -> Result<Vec<PlanPtr>, CompileError> {
        use crate::grammar::Expr::*;
        use Binding::*;
        match e {
            EAnf(_, a) => self.plan_anf(ctx, a),
            EPrj(_, i, a) => {
                self.binding = Alias;
                Ok(vec![self.plan_anf(ctx, a)?[*i]])
            }
            EFst(_, a) => {
                self.binding = Alias;
                Ok(vec![self.plan_anf(ctx, a)?[0]])
            }
            ESnd(_, a) => {
                self.binding = Alias;
                Ok(vec![self.plan_anf(ctx, a)?[1]])
            }
            EProd(_, az) => {
                let span = tracing::span!(tracing::Level::DEBUG, "prod");
                let _enter = span.enter();
                self.plan_anfs(ctx, az)
            }
            ELetIn(v, s, ebound, ebody) => {
                if v.id().0 > self.mx_uid.0 {
                    self.mx_uid = MaxUniqueId(v.id().0);
                }
                let span = tracing::span!(tracing::Level::DEBUG, "let");
                let _enter = span.enter();
                debug!("{:?}", v);

                let binding = match &self.binding {
                    Query | CompoundQuery(_) => panic!("impossible"),
                    Alias | Unbound | Let(_) => Let(v.clone()),
                    NamedSample(prv) | NamedSampleLet(prv, _) => {
                        NamedSampleLet(prv.clone(), v.clone())
                    }
                };
                self.binding = binding.clone();
                let ptrs = self.plan_expr(ctx, ebound)?;
                ctx.insert_sub(v.clone(), ptrs.clone());

                if ![Alias, Query].contains(&self.binding) {
                    for ptr in &ptrs {
                        let deps = self.mgr.flatten(*ptr);
                        self.graph.insert_edge(&deps, self.binding.clone());
                    }
                }
                self.binding = binding;

                // perform look ahead for the query
                match **ebody {
                    ELetIn(_, _, _, _) => {
                        let x = self.plan_expr(ctx, ebody)?;
                        Ok(x)
                    }
                    _ => {
                        let binding = self.binding.clone();
                        self.binding = Query;
                        let ret = self.plan_expr(ctx, ebody)?;
                        match &self.binding {
                            CompoundQuery(0) => {
                                for (i, p) in ret.iter().enumerate() {
                                    let deps = self.mgr.flatten(*p);
                                    self.graph.insert_edge(&deps, CompoundQuery(i));
                                }
                            }
                            CompoundQuery(_) => panic!("impossible!"),
                            _ => {}
                        }
                        Ok(ret)
                    }
                }
            }
            EIte(_, cond, t, f) => {
                let ps = self.plan_anf(ctx, cond)?;
                let ts = self.plan_expr(ctx, t)?;
                let fs = self.plan_expr(ctx, f)?;
                let out = izip!(ps, ts, fs)
                    .map(|(p, t, f)| self.mgr.ite(p, t, f))
                    .collect_vec();
                // self.graph
                //     .insert_edge(&out.iter().cloned().collect(), self.binding.clone());
                Ok(out)
            }
            EFlip(v, param) => {
                if v.label.value() > self.mx_lbl.0 {
                    self.mx_lbl = MaxVarLabel(v.label.value());
                }
                if v.id().0 > self.mx_uid.0 {
                    self.mx_uid = MaxUniqueId(v.id().0);
                }
                let ptr = self.mgr.var(v.label, true);
                self.graph.insert_vertex(ptr, ());
                // self.graph
                //     .insert_edge(&HashSet::from([ptr]), self.binding.clone());

                Ok(vec![ptr])
            }
            EObserve(_, a) => self.plan_anf(ctx, a),
            ESample(_, e) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample");
                let _enter = span.enter();

                // let initial_binding = self.binding.clone();
                let binding = match &self.binding {
                    Let(prv) => NamedSample(prv.clone()),
                    x => x.clone(),
                };
                self.binding = binding;
                let ret = self.plan_expr(ctx, e);
                // self.binding = initial_binding;
                ret
            }
        }
    }
    pub fn plan(
        &mut self,
        p: &ProgramAnn,
    ) -> Result<(IteractionGraph, MaxUniqueId, MaxVarLabel), CompileError> {
        match p {
            Program::Body(b) => {
                let mut ctx = Default::default();
                let _ = self.plan_expr(&mut ctx, b)?;
                Ok((self.graph.clone(), self.mx_uid, self.mx_lbl))
            }
        }
    }
}

pub fn pipeline(
    p: &crate::ProgramInferable,
) -> Result<(IteractionGraph, MaxUniqueId, MaxVarLabel), CompileError> {
    let (p, vo, varmap, inv, mxlbl) = crate::annotate::pipeline(p)?;
    let mut env = InteractionEnv::default();
    env.plan(&p)
}

#[derive(Clone, Copy, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rank(pub usize);
pub fn order_cuts(g: &IteractionGraph) -> Vec<(Binding, Rank)> {
    let mut ranking = vec![];
    let mut sorted_edges = g.rank_edge_cuts();
    sorted_edges.sort_by(|(_, _, a), (_, _, b)| b.cmp(&a));
    for (edges, label, rank) in sorted_edges {
        ranking.push((label.clone(), rank));
    }
    ranking
}
pub fn top_k_cuts(cuts: &Vec<(Binding, Rank)>, n: usize) -> Vec<Binding> {
    assert!(
        n - 1 < cuts.len(),
        "requested {} cuts, but only {} possible cut{} found",
        n,
        cuts.len(),
        String::from(if cuts.len() == 1 { "" } else { "s" })
    );
    cuts.iter().map(|(b, r)| b.clone()).take(n).collect()
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
    }

    #[test]
    pub fn test_interaction_graph_works_with_conjoined_query() {
        let p = program!(lets![
            "x" ;= flip!(0.0);
            "y" ;= flip!(0.0);
           ...? b!("x" && "y")
        ]);
        let (g, uid, lbl) = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 2);
        println!("{}", g.print());
        assert_eq!(g.hyperedges.len(), 3, "each line + complex query");
        let cuts = order_cuts(&g);
        assert_eq!(cuts.len(), 3);
        let pann = crate::annotate::pipeline(&p).unwrap().0;
        let cs = top_k_cuts(&cuts, 1);
        for c in &cuts {
            println!("{:?}", c);
        }
    }

    #[test]
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
    // #[traced_test]
    // #[ignore = "expectations need to be revisited"]
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
