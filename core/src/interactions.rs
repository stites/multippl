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
pub struct PlanPtr(usize);

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
    vertices: HashSet<(V, VL)>,
    hyperedges: Vec<(HashSet<V>, EL)>,
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

impl<V, VL, EL> Hypergraph<V, VL, EL>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
    VL: Clone + Debug + PartialEq + Eq + Hash,
    EL: Clone + Debug + PartialEq + Eq + Hash,
{
    /// add an edge to the hypergraph. Returns false if the edge is already in the hypergraph
    pub fn insert_edge(&mut self, edge: &HashSet<V>, l: EL) -> bool {
        debug!("insert edge: {:?} @ {:?}", edge, l);

        let verts: HashSet<&V> = self.vertices.iter().map(|x| &x.0).collect();
        let edgerefs: HashSet<&V> = edge.iter().collect();
        let new_verts: HashSet<&V> = edgerefs.difference(&verts).cloned().map(|x| x).collect();
        if !new_verts.is_empty() {
            panic!("not all vertices in graph: {:?}", new_verts);
        }

        let _next_ix = self.hyperedges.len();
        match self.hyperedges.iter().find(|e| e.0 == *edge && e.1 == l) {
            Some(_) => return false,
            None => self.hyperedges.push((edge.clone(), l)),
        }
        true
    }
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
    Let(NamedVar),
    NamedSample(NamedVar),
    NamedSampleLet(NamedVar, NamedVar),
    Query,
    CompoundQuery,
    // TODO: beyond the prototype, these would be warranted:
    // InUnnamedSample,
    // InUnnamedIteBranch(bool),
    // NamedIteBranch(bool, NamedVar),
}
impl Binding {
    pub fn id(&self) -> Option<UniqueId> {
        use Binding::*;
        match self {
            Unbound | Query | CompoundQuery => None,
            Let(v) | NamedSample(v) | NamedSampleLet(_, v) => Some(v.id()),
        }
    }
    pub fn sample_id(&self) -> Option<UniqueId> {
        use Binding::*;
        match self {
            Unbound | Query | CompoundQuery | Let(_) => None,
            NamedSample(v) | NamedSampleLet(v, _) => Some(v.id()),
        }
    }
    pub fn cut_id(&self) -> Option<UniqueId> {
        use Binding::*;
        match self {
            Unbound | Query | CompoundQuery | NamedSample(_) | NamedSampleLet(_, _) => None,
            Let(v) => Some(v.id()),
        }
    }
}

pub struct HypStore {
    graph: Hypergraph<PlanPtr, (), Binding>,
    label_info: HashMap<VarLabel, (PlanPtr, Binding)>,
    mgr: PlanManager,
}
impl Default for HypStore {
    fn default() -> Self {
        HypStore {
            mgr: Default::default(),
            graph: Default::default(),
            label_info: Default::default(),
        }
    }
}

impl HypStore {
    // variables, created at flip, must be instatiated at a let-binding
    pub fn new_var(&mut self, v: VarLabel, l: &Binding) -> PlanPtr {
        let ptr = self.mgr.var(v, true);
        self.graph.insert_vertex(ptr, ());
        self.label_info.insert(v, (ptr, l.clone()));
        self.graph.insert_edge(&HashSet::from([ptr]), l.clone());
        ptr
    }
    // variables, created at flip, must be instatiated at a let-binding
    pub fn get_var(&mut self, v: VarLabel) -> PlanPtr {
        self.label_info.get(&v).unwrap().0
    }
    // and-expressions may happen at any cutsite
    pub fn and(&mut self, l: PlanPtr, r: PlanPtr) -> PlanPtr {
        let ptr = self.mgr.and(l, r);
        // self.graph
        //     .insert_edge(&self.mgr.flatten_all(&[l, r]), Binding::Unbound);
        ptr
    }
    // or-expressions may happen at any cutsite
    pub fn or(&mut self, l: PlanPtr, r: PlanPtr) -> PlanPtr {
        let ptr = self.mgr.or(l, r);
        // self.graph
        //     .insert_edge(&self.mgr.flatten_all(&[l, r]), Binding::Unbound);
        ptr
    }

    // ite either happens at a top-level let-binding or not.
    // TODO: we could add cutsites to each branch as well
    pub fn ite(&mut self, p: PlanPtr, t: PlanPtr, f: PlanPtr, lbl: &Binding) -> PlanPtr {
        let ptr = self.mgr.ite(p, t, f);
        // self.graph
        //     .insert_edge(&self.mgr.flatten_all(&[p, t, f]), lbl.clone());
        ptr
    }
    pub fn not(&mut self, p: PlanPtr) -> PlanPtr {
        self.mgr.not(p)
    }
    pub fn bool(&mut self, p: bool) -> PlanPtr {
        self.mgr.bool(p)
    }
}

pub struct InteractionEnv {
    mgr: HypStore,
    binding: Binding,
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
            binding: Binding::Unbound,
        }
    }
}
impl InteractionEnv {
    fn plan_anf(&mut self, ctx: &Ctx, a: &AnfAnn) -> Result<Vec<PlanPtr>, CompileError> {
        use crate::grammar::Anf::*;
        match a {
            AVar(v, s) => match ctx.substitutions.get(&v) {
                None => panic!("must already be included"),
                Some(ptrs) => Ok(ptrs.clone()),
            },
            AVal(_, Val::Bool(b)) => Ok(vec![self.mgr.bool(*b)]),
            AVal(_, Val::Prod(_)) => todo!(),
            And(bl, br) => {
                let b = match &self.binding {
                    Binding::Query => Binding::CompoundQuery,
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
                    Binding::Query => Binding::CompoundQuery,
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
                let deps = self.mgr.mgr.flatten(res[0]);
                debug!("print: {:?}", a);
                // self.mgr.graph.insert_edge(&deps, self.binding.clone());
                Ok(res[0])
            })
            .collect()
    }
    fn plan_expr(&mut self, ctx: &mut Ctx, e: &ExprAnn) -> Result<Vec<PlanPtr>, CompileError> {
        use crate::grammar::Expr::*;
        use Binding::*;
        match e {
            EAnf(_, a) => {
                let binding = self.binding.clone();
                let ptrs = self.plan_anf(ctx, a)?;
                if binding != Query {
                    for p in &ptrs {
                        let deps = self.mgr.mgr.flatten(*p);
                        self.mgr.graph.insert_edge(&deps, self.binding.clone());
                    }
                }
                self.binding = binding;
                Ok(ptrs)
            }
            EPrj(_, i, a) => Ok(vec![self.plan_anf(ctx, a)?[*i]]),
            EFst(_, a) => Ok(vec![self.plan_anf(ctx, a)?[0]]),
            ESnd(_, a) => Ok(vec![self.plan_anf(ctx, a)?[1]]),
            EProd(_, az) => {
                let span = tracing::span!(tracing::Level::DEBUG, "prod");
                let _enter = span.enter();
                self.plan_anfs(ctx, az)
            }
            ELetIn(v, s, ebound, ebody) => {
                let span = tracing::span!(tracing::Level::DEBUG, "let");
                let _enter = span.enter();
                debug!("{:?}", v);

                let binding = match &self.binding {
                    Query | CompoundQuery => panic!("impossible"),
                    Unbound | Let(_) => Let(v.clone()),
                    NamedSample(prv) | NamedSampleLet(prv, _) => {
                        NamedSampleLet(prv.clone(), v.clone())
                    }
                };
                self.binding = binding;
                let ptrs = self.plan_expr(ctx, ebound)?;
                ctx.insert_sub(v.clone(), ptrs);
                // perform look ahead for the query
                match **ebody {
                    ELetIn(_, _, _, _) => self.plan_expr(ctx, ebody),
                    _ => {
                        self.binding = Query;
                        self.plan_expr(ctx, ebody)
                    }
                }
            }
            EIte(_, cond, t, f) => {
                let ps = self.plan_anf(ctx, cond)?;
                let ts = self.plan_expr(ctx, t)?;
                let fs = self.plan_expr(ctx, f)?;
                Ok(izip!(ps, ts, fs)
                    .map(|(p, t, f)| self.mgr.ite(p, t, f, &self.binding))
                    .collect_vec())
            }
            EFlip(v, param) => Ok(vec![self.mgr.new_var(v.label, &self.binding)]),
            EObserve(_, a) => self.plan_anf(ctx, a),
            ESample(_, e) => {
                let span = tracing::span!(tracing::Level::DEBUG, "sample");
                let _enter = span.enter();

                let initial_binding = self.binding.clone();
                let binding = match &self.binding {
                    Let(prv) => NamedSample(prv.clone()),
                    x => x.clone(),
                };
                self.binding = binding;
                let ret = self.plan_expr(ctx, e);
                self.binding = initial_binding;
                ret
            }
        }
    }
    pub fn plan(&mut self, p: &ProgramAnn) -> Result<IteractionGraph, CompileError> {
        match p {
            Program::Body(b) => {
                let mut ctx = Default::default();
                let _ = self.plan_expr(&mut ctx, b)?;
                Ok(self.mgr.graph.clone())
            }
        }
    }
}

pub fn pipeline(p: &crate::ProgramTyped) -> Result<IteractionGraph, CompileError> {
    let (p, vo, varmap, inv, mxlbl) = crate::annotate::pipeline(p)?;
    let mut env = InteractionEnv::default();
    env.plan(&p)
}

pub struct Rank(pub usize);
pub fn order_cuts(g: &IteractionGraph) -> Vec<(Binding, Rank)> {
    let mut ranking = vec![];
    let mut sorted_covers = g.covers();
    sorted_covers.sort_by(|(a, _), (b, _)| b.len().cmp(&a.len()));
    for (cover, edges) in sorted_covers {
        for (_, label) in edges {
            ranking.push((label.clone(), Rank(cover.len())));
        }
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
pub fn apply_cuts(cuts: Vec<Binding>, p: &ProgramAnn) -> ProgramAnn {
    cuts.iter().fold(p.clone(), |fin, cut| apply_cut(cut, &fin))
}
pub fn apply_cut(cut: &Binding, p: &ProgramAnn) -> ProgramAnn {
    sample_program(cut, p)
}

fn sample_expr(cut: &Binding, e: &ExprAnn) -> ExprAnn {
    use crate::grammar::Expr::*;
    use Binding::*;
    match e {
        // EAnf(_, a) => e.clone(),
        // EPrj(_, i, a) => e.clone(),
        // EFst(_, a) => e.clone(),
        // ESnd(_, a) => e.clone(),
        // EProd(_, az) => {
        //     // let span = tracing::span!(tracing::Level::DEBUG, "prod");
        //     // let _enter = span.enter();
        //     // sample_anfs(az)
        //     e.clone()
        // }
        ELetIn(v, s, ebound, ebody) => match cut.cut_id().map(|i| v.id() == i) {
            Some(true) => {
                let smpl = ESample((), ebound.clone());
                ELetIn(v.clone(), s.to_string(), Box::new(smpl), ebody.clone())
            }
            _ => e.clone(),
        },
        _ => e.clone(),
        // EIte(_, cond, t, f) => {
        //     let ps = sample_anf(cond)?;
        //     let ts = sample_expr(t)?;
        //     let fs = sample_expr(f)?;
        //     None
        // }
        // EFlip(v, param) => None,
        // EObserve(_, a) => sample_anf(a),
        // ESample(_, e) => {
        //     let span = tracing::span!(tracing::Level::DEBUG, "sample");
        //     let _enter = span.enter();
        //     sample_expr(e)
        // }
        // _ => todo!(""),
    }
}
pub fn sample_program(cut: &Binding, p: &ProgramAnn) -> ProgramAnn {
    match p {
        Program::Body(b) => Program::Body(sample_expr(cut, b)),
    }
}

pub fn rebind_expressions(g: &IteractionGraph) -> Vec<(Binding, Rank)> {
    todo!("probably just skip sampling unbounded expressions for now")
}
pub fn tuple_samples(g: &IteractionGraph) -> Vec<(Binding, Rank)> {
    todo!("important when we get multi-rooted WMC")
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
    pub fn test_interaction_graph_for_simple_program_todo() {
        let p = program!(lets![
            "x" ; b!() ;= flip!(1/3);
           ...? b!("x") ; b!()
        ]);
        let g = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 1);
        assert_eq!(g.hyperedges.len(), 1);
        let (_, nvar) = g.hyperedges[0].clone();
        assert_eq!(nvar, Binding::Let(named(0, "x")), "flip edge first");
        let cs = order_cuts(&g);
        let pann = crate::annotate::pipeline(&p).unwrap().0;
        let p1 = apply_cuts(&cs, 1, &pann);
        let p1_expected = program!(lets![
            "x" ; b!() ;= sample!(flip!(1/3));
           ...? b!("x") ; b!()
        ]);
        let pann_expected = crate::annotate::pipeline(&p1_expected).unwrap().0;
        assert_eq!(&p1, &pann_expected);
        let p2 = apply_cuts(&cs, 2, &pann);
        assert_eq!(&p2, &pann_expected);
    }
    #[test]
    #[ignore]
    pub fn test_interaction_graph_with_boolean_operator() {
        let p = program!(lets![
            "x" ; b!() ;= flip!(1/3);
            "y" ; b!() ;= flip!(1/3);
            "z" ; b!() ;= b!("x" && "y");
           ...? b!("z") ; b!()
        ]);
        let g = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 2);
        assert_eq!(g.hyperedges.len(), 4, "edge for each line");
        for (ix, (id, s)) in [(0, "x"), (2, "y"), (4, "z")].iter().enumerate() {
            let (_, nvar) = g.hyperedges[ix].clone();
            assert_eq!(nvar, Binding::Let(named(*id, s)));
        }
        let (_, nvar) = g.hyperedges.last().clone().unwrap();
        assert_eq!(*nvar, Binding::Unbound);
        let order = order_cuts(&g);
        assert_eq!(order.len(), 1);
    }
    #[test]
    #[ignore]
    pub fn test_interaction_graph_works_with_conjoined_query_todo() {
        let p = program!(lets![
            "x" ; b!() ;= flip!(1/3);
            "y" ; b!() ;= flip!(1/3);
           ...? b!("x" && "y") ; b!()
        ]);
        let g = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 2);
        assert_eq!(g.hyperedges.len(), 3, "edge for each line");
        order_cuts(&g);
    }
    #[test]
    #[ignore]
    pub fn test_interaction_graph_captures_correct_edge_with_aliases_todo() {
        let p = program!(lets![
            "x" ; b!() ;= flip!(1/3);
            "y" ; b!() ;= flip!(1/3);
            "z" ; b!() ;= flip!(1/3);
            "a" ; b!() ;= b!("x" && "y" && "z");
           ...? b!("a") ; b!()
        ]);
        let g = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 3);
        assert_eq!(g.hyperedges.len(), 5, "edge for each line");
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 3, "query depends on every variable above");
        order_cuts(&g);
    }

    #[test]
    #[ignore]
    pub fn test_interaction_works_as_expected_for_samples_todo() {
        let shared_var = program!(lets![
           "x" ; b!() ;= flip!(1/3);
           "l" ; b!() ;= sample!(var!("x"));
           "r" ; b!() ;= sample!(var!("x"));
           ...? b!("r") ; b!()
        ]);
        let g = pipeline(&shared_var).unwrap();
        assert_eq!(g.vertices.len(), 1);
        let es = g.hyperedges.clone();
        assert_eq!(g.hyperedges.len(), 4, "edge for each line");
        let n = &es[0].1;
        assert_eq!(n, &Binding::Let(named(0, "x")));
        let (e, n) = &es[1];
        assert_eq!(n, &Binding::NamedSample(named(2, "l")));
        assert_eq!(e, &HashSet::from([PlanPtr(0)]));
        let (e, n) = &es[2];
        assert_eq!(n, &Binding::NamedSample(named(3, "r")));
        assert_eq!(e, &HashSet::from([PlanPtr(0)]));
        let (query, n) = &es[3];
        assert_eq!(n, &Binding::Unbound);
        assert_eq!(query, &HashSet::from([PlanPtr(0)]));
        order_cuts(&g);
    }

    #[test]
    #[ignore]
    pub fn test_interaction_shared_tuples_get_separated_todo() {
        let shared_tuple = program!(lets![
           "x" ; b!()     ;= flip!(1/3);
           "y" ; b!()     ;= flip!(1/3);
           "z" ; b!(B, B) ;= sample!(b!("x", "y"));
           ...? b!("z" ; b!(B, B)); b!(B, B)
        ]);
        let g = pipeline(&shared_tuple).unwrap();
        assert_eq!(g.vertices.len(), 2);
        // for (edge, nvar) in &g.hyperedges {
        //     println!("nvar: {:?}", nvar);
        //     println!("edge: {:?}", edge);
        // }
        assert_eq!(g.hyperedges.len(), 4, "needs a tuple for each position");
        order_cuts(&g);
    }

    #[test]
    #[ignore]
    pub fn test_interaction_ite_sample_todo() {
        let ite = program!(lets![
            "x" ; b!() ;= flip!(1/5);
            "y" ; b!() ;= ite!(
                if ( var!("x") )
                then { sample!(flip!(1/3)) }
                else { flip!(1/4) });
            ...? b!("y") ; b!()
        ]);
        let g = pipeline(&ite).unwrap();
        assert_eq!(g.vertices.len(), 3);
        assert_eq!(g.hyperedges.len(), 4, "edge for each line");
        for (edge, nvar) in &g.hyperedges {
            println!("nvar: {:?}", nvar);
            println!("edge: {:?}", edge);
        }
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 3, "query depends on every variable above");
        order_cuts(&g);
    }

    #[test]
    #[ignore]
    // #[traced_test]
    pub fn test_interaction_ite_nested_let_todo() {
        let ite_with_nested_lets = program!(lets![
            "x" ; b!() ;= flip!(2/3);
            "y" ; b!() ;= ite!(
                if ( var!("x") )
                    then { lets![
                             "q" ; b!() ;= flip!(1/4);
                             "_" ; b!() ;= observe!(b!("q" || "x"));
                             ...? b!("q") ; b!()
                    ] }
                else { flip!(1/5) });
            "_" ; b!() ;= observe!(b!("x" || "y"));
            ...? b!("x") ; b!()
        ]);
        let g = pipeline(&ite_with_nested_lets).unwrap();
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
    #[ignore]
    pub fn test_interaction_2x2_triu_todo() {
        let grid2x2_triu = program!(lets![
            "00" ; B!() ;= flip!(1/2);
            "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            ...? b!("01", "10") ; B!()
        ]);
        let g = pipeline(&grid2x2_triu).unwrap();
        assert_eq!(g.vertices.len(), 5);
        assert_eq!(g.hyperedges.len(), 5, "edge for each line");
        order_cuts(&g);
    }

    #[test]
    #[ignore]
    pub fn test_interaction_2x2_tril_todo() {
        let grid2x2_tril = program!(lets![
            "01" ; B!() ;= flip!(1/3) ;
            "10" ; B!() ;= flip!(1/4) ;
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(3/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(3/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(3/9) ) : (
                                                          flip!(3/11) ))))));
            ...? b!("11") ; B!()
        ]);
        let g = pipeline(&grid2x2_tril).unwrap();
        assert_eq!(g.vertices.len(), 6);
        assert_eq!(g.hyperedges.len(), 7, "edge for each var and full ITE");
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 6, "query depends on every variable above");
        order_cuts(&g);
    }

    #[test]
    #[ignore]
    pub fn test_interaction_2x2_full_todo() {
        let grid2x2 = program!(lets![
            "00" ; B!() ;= flip!(1/2);
            "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                          flip!(1/11) ))))));
            ...? b!("11") ; B!()
        ]);
        let g = pipeline(&grid2x2).unwrap();
        assert_eq!(g.vertices.len(), 9);
        assert_eq!(g.hyperedges.len(), 10, "vertex + ITE");
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 9, "query depends on every variable above");
        todo!("actually, this one seems wrong: NamedVar(11) seems to miss the ITE");
        order_cuts(&g);
    }

    #[test]
    #[ignore]
    pub fn test_interaction_grid2x2_sampled_todo() {
        let grid2x2_sampled = program!(lets![
            "00" ; B!() ;= flip!(1/2);
            "01_10" ; b!(B, B) ;= sample!(
                lets![
                    "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
                    "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
                    ...? b!("01", "10") ; b!(B, B)
                ]);
            "01" ; B!() ;= fst!("01_10");
            "10" ; B!() ;= snd!("01_10");
            "11" ; B!() ;=
                ite!(( b!((  b!(@anf "10")) && (  b!(@anf "01"))) ) ? ( flip!(1/7) ) : (
                ite!(( b!((  b!(@anf "10")) && (not!("01"))) ) ? ( flip!(1/8) ) : (
                ite!(( b!((  not!("10")) && (  b!(@anf "01"))) ) ? ( flip!(1/9) ) : (
                                                          flip!(1/11) ))))));
            ...? b!("11") ; B!()
        ]);
        let g = pipeline(&grid2x2_sampled).unwrap();
        assert_eq!(g.vertices.len(), 9);
        assert_eq!(g.hyperedges.len(), 10, "vertex + ITE");
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 9, "query depends on every variable above");
        order_cuts(&g);
    }
}
