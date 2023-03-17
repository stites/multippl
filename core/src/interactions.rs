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

impl<V, VL, EL> Hypergraph<V, VL, EL>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
    VL: Clone + Debug + PartialEq + Eq + Hash,
    EL: Clone + Debug + PartialEq + Eq + Hash,
{
    /// add an edge to the hypergraph. Returns false if the edge is already in the hypergraph
    pub fn insert_edge(&mut self, edge: &HashSet<V>, l: EL) -> bool {
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
}

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub enum Binding {
    Unbound,
    Let(NamedVar),
    InNamedSample(NamedVar),
    InNamedSampleLet(NamedVar, NamedVar),
    // TODO: beyond the prototype, these would be warranted:
    // InUnnamedSample,
    // InUnnamedIteBranch(bool),
    // InNamedIteBranch(bool, NamedVar),
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
                let pls = self.plan_anf(ctx, bl)?;
                let prs = self.plan_anf(ctx, br)?;
                let ret = izip!(pls, prs)
                    .map(|(l, r)| self.mgr.and(l, r))
                    .collect_vec();
                Ok(ret)
            }
            Or(bl, br) => {
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
                self.mgr.graph.insert_edge(&deps, self.binding.clone());
                Ok(res[0])
            })
            .collect()
    }
    fn plan_expr(&mut self, ctx: &mut Ctx, e: &ExprAnn) -> Result<Vec<PlanPtr>, CompileError> {
        use crate::grammar::Expr::*;
        use Binding::*;
        match e {
            EAnf(_, a) => {
                let ptrs = self.plan_anf(ctx, a)?;
                for p in &ptrs {
                    let deps = self.mgr.mgr.flatten(*p);
                    self.mgr.graph.insert_edge(&deps, self.binding.clone());
                }
                Ok(ptrs)
            }
            EPrj(_, i, a) => Ok(vec![self.plan_anf(ctx, a)?[*i]]),
            EFst(_, a) => Ok(vec![self.plan_anf(ctx, a)?[0]]),
            ESnd(_, a) => Ok(vec![self.plan_anf(ctx, a)?[1]]),
            EProd(_, az) => self.plan_anfs(ctx, az),
            ELetIn(v, s, ebound, ebody) => {
                let binding = match &self.binding {
                    Unbound | Let(_) => Let(v.clone()),
                    InNamedSample(prv) | InNamedSampleLet(prv, _) => {
                        InNamedSampleLet(prv.clone(), v.clone())
                    }
                };
                self.binding = binding;
                let ptrs = self.plan_expr(ctx, ebound)?;
                ctx.insert_sub(v.clone(), ptrs);
                // perform look ahead for the query
                match **ebody {
                    ELetIn(_, _, _, _) => self.plan_expr(ctx, ebody),
                    _ => {
                        self.binding = Unbound;
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
                let binding = match &self.binding {
                    Let(prv) => InNamedSample(prv.clone()),
                    x => x.clone(),
                };
                self.binding = binding;
                self.plan_expr(ctx, e)
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
            "x" ; b!() ;= flip!(1/3);
           ...? b!("x") ; b!()
        ]);
        let g = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 1);
        assert_eq!(g.hyperedges.len(), 2, "expected a flip and query edge");
        let (_, nvar) = g.hyperedges[0].clone();
        assert_eq!(nvar, Binding::Let(named(0, "x")), "flip edge first");
        let (_, nvar) = g.hyperedges[1].clone();
        assert_eq!(nvar, Binding::Unbound, "expected query edge last");
    }
    #[test]
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
    }
    #[test]
    pub fn test_interaction_graph_works_with_conjoined_query() {
        let p = program!(lets![
            "x" ; b!() ;= flip!(1/3);
            "y" ; b!() ;= flip!(1/3);
           ...? b!("x" && "y") ; b!()
        ]);
        let g = pipeline(&p).unwrap();
        assert_eq!(g.vertices.len(), 2);
        assert_eq!(g.hyperedges.len(), 3, "edge for each line");
    }
    #[test]
    pub fn test_interaction_graph_captures_correct_edge_with_aliases() {
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
        // for (edge, nvar) in &g.hyperedges {
        //     println!("nvar: {:?}", nvar);
        //     println!("edge: {:?}", edge);
        // }
        let query = &g.hyperedges.last().unwrap().0;
        assert_eq!(query.len(), 3, "query depends on every variable above");
    }

    #[test]
    pub fn test_interaction_todos() {
        // <<< <<< <<< <<< <<< <<< <<< <<< <<< <<< progress
        let shared_var = program!(lets![
           "x" ; b!() ;= flip!(1/3);
           "l" ; b!() ;= sample!(var!("x"));
           "r" ; b!() ;= sample!(var!("x"));
           ...? b!("r") ; b!()
        ]);
        let g = pipeline(&shared_var).unwrap();

        let shared_tuple = program!(lets![
           "x" ; b!()     ;= flip!(1/3);
           "z" ; b!(B, B) ;= sample!(b!("x", "x"));
           ...? b!("z" ; b!(B, B)); b!(B, B)
        ]);
        let g = pipeline(&shared_var).unwrap();
        let ite = program!(lets![
            "x" ; b!() ;= flip!(1/5);
            "y" ; b!() ;= ite!(
                if ( var!("x") )
                then { sample!(flip!(1/3)) }
                else { flip!(1/4) });
            ...? b!("y") ; b!()
        ]);
        let g = pipeline(&ite).unwrap();
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
        let grid2x2_triu = program!(lets![
            "00" ; B!() ;= flip!(1/2);
            "01" ; B!() ;= ite!( ( b!(@anf "00")  ) ? ( flip!(1/3) ) : ( flip!(1/4) ) );
            "10" ; B!() ;= ite!( ( not!("00") ) ? ( flip!(1/5) ) : ( flip!(1/6) ) );
            ...? b!("01", "10") ; B!()
        ]);
        let g = pipeline(&grid2x2_triu).unwrap();
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
    }
}
