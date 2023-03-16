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
struct PlanManager {
    gensym: usize,
    store: HashMap<PlanPtr, PlanNode>,
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
pub enum CutSite {
    LetIn(NamedVar),
    // TODO do I need a "sampled" statement which cannot be cut?
    // SampledLetIn(NamedVar), // cannot be cut
    // IteTrue(NamedVar),
    // IteFalse(NamedVar),
}

struct HypStore<'a> {
    graph: Hypergraph<PlanPtr, (), Option<CutSite>>,
    label_info: HashMap<VarLabel, (PlanPtr, NamedVar)>,
    mgr: &'a mut PlanManager,
}
impl<'a> HypStore<'a> {
    // variables, created at flip, must be instatiated at a let-binding
    pub fn new_var(&mut self, v: VarLabel, l: NamedVar) -> PlanPtr {
        let ptr = self.mgr.var(v, true);
        self.graph.insert_vertex(ptr, ());
        self.label_info.insert(v, (ptr, l.clone()));
        self.graph
            .insert_edge(&HashSet::from([ptr]), Some(CutSite::LetIn(l)));
        ptr
    }
    // variables, created at flip, must be instatiated at a let-binding
    pub fn get_var(&mut self, v: VarLabel) -> PlanPtr {
        self.label_info.get(&v).unwrap().0
    }
    // and-expressions may happen at any cutsite
    pub fn and(&mut self, l: PlanPtr, r: PlanPtr) -> PlanPtr {
        let ptr = self.mgr.and(l, r);
        self.graph.insert_edge(&self.mgr.flatten_all(&[l, r]), None);
        ptr
    }
    // or-expressions may happen at any cutsite
    pub fn or(&mut self, l: PlanPtr, r: PlanPtr) -> PlanPtr {
        let ptr = self.mgr.or(l, r);
        self.graph.insert_edge(&self.mgr.flatten_all(&[l, r]), None);
        ptr
    }
    // ite either happens at a top-level let-binding or not.
    // TODO: we could add cutsites to each branch as well
    pub fn ite(&mut self, p: PlanPtr, t: PlanPtr, f: PlanPtr, lbl: Option<NamedVar>) -> PlanPtr {
        let ptr = self.mgr.ite(p, t, f);
        // self.graph.insert_edge(
        //     &self.mgr.flatten(t),
        //     lbl.as_ref().map(|x| CutSite::IteTrue(x.clone())),
        // );
        // self.graph.insert_edge(
        //     &self.mgr.flatten(f),
        //     lbl.as_ref().map(|x| CutSite::IteFalse(x.clone())),
        // );
        self.graph.insert_edge(
            &self.mgr.flatten_all(&[p, t, f]),
            lbl.as_ref().map(|x| CutSite::LetIn(x.clone())),
        );
        ptr
    }
    pub fn not(&mut self, p: PlanPtr) -> PlanPtr {
        self.mgr.not(p)
    }
    pub fn bool(&mut self, p: bool) -> PlanPtr {
        self.mgr.bool(p)
    }
}

pub struct InteractionEnv<'a> {
    gensym: usize,
    mgr: HypStore<'a>,
}

struct Ctx {
    substitutions: HashMap<NamedVar, Vec<PlanPtr>>,
    substitutions_id: HashMap<UniqueId, Vec<PlanPtr>>,
}

struct Output {
    substitutions: HashMap<NamedVar, Vec<PlanPtr>>,
    substitutions_id: HashMap<UniqueId, Vec<PlanPtr>>,
}
impl Output {
    fn from_ctx(ctx: &Ctx) -> Self {
        Self {
            substitutions: ctx.substitutions.clone(),
            substitutions_id: ctx.substitutions_id.clone(),
        }
    }
}

impl<'a> InteractionEnv<'a> {
    fn new(hypstore: HypStore<'a>) -> InteractionEnv<'a> {
        Self {
            gensym: 0,
            mgr: hypstore,
        }
    }
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
                let prs = self.plan_anf(ctx, bl)?;
                let ret = izip!(pls, prs)
                    .map(|(l, r)| self.mgr.and(l, r))
                    .collect_vec();
                Ok(ret)
            }
            Or(bl, br) => {
                let pls = self.plan_anf(ctx, bl)?;
                let prs = self.plan_anf(ctx, bl)?;
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
                Ok(res[0])
            })
            .collect()
    }
    // fn plan_expr(&mut self, ctx: &Ctx, e: &ExprAnn) -> Result<Vec<PlanPtr>, CompileError> {
    //     use crate::grammar::Expr::*;
    //     match e {
    //         EAnf(_, a) => self.plan_anf(ctx, a),
    //         EPrj(_, i, a) => { Ok(vec![self.plan_anf(ctx, a)?[*i]]) },
    //         EFst(_, a) => { Ok(vec![self.plan_anf(ctx, a)?[0]]) },
    //         ESnd(_, a) => { Ok(vec![self.plan_anf(ctx, a)?[1]]) },
    //         EProd(_, az) => self.plan_anfs(ctx, az),
    //         ELetIn(v, s, ebound, ebody) => {
    //             use SamplingContext::*;
    //             self.state = Dependence;
    //             match &self.sampling_context {
    //                 Unset | Set(_) => {
    //                     self.sampling_context = Set(v.clone());
    //                 }
    //                 Sampling(prv) => {
    //                     self.sampling_context = SamplingWithLet(prv.clone(), v.clone());
    //                 }
    //                 SamplingWithLet(prv, _) => {
    //                     self.sampling_context = SamplingWithLet(prv.clone(), v.clone());
    //                 }
    //             }
    //             self.plan_expr(ebound)?;
    //             self.plan_expr(ebody)?;
    //             Ok(())
    //         }
    //         EIte(_ty, cond, t, f) => {
    //             self.state = Dependence;
    //             self.plan_anf(cond)?;
    //             self.plan_expr(t)?;
    //             self.plan_expr(f)?;
    //             Ok(())
    //         }
    //         EFlip(v, param) => Ok(()),
    //         EObserve(_, a) => {
    //             self.state = Dependence;
    //             self.plan_anf(a)
    //         }
    //         ESample(_, e) => {
    //             use SamplingContext::*;

    //             match &self.sampling_context {
    //                 Sampling(_) | Unset => {}
    //                 Set(v) => {
    //                     self.sampling_context = Sampling(v.clone());
    //                 }
    //                 SamplingWithLet(_, v) => {
    //                     self.sampling_context = Sampling(v.clone());
    //                 }
    //             }

    //             self.plan_expr(e)
    //         }
    //     }
    // }
    //     // // pub fn decorate_anf(&mut self, a: &AnfAnn) -> Result<AnfAnlys, CompileError> {
    //     // //     use crate::grammar::Anf::*;
    //     // //     match a {
    //     // //         AVar(v, s) => match self.decor.get(v) {
    //     // //             None => panic!("impossible"),
    //     // //             Some(dv) => Ok(AVar(dv.clone(), s.to_string())),
    //     // //         },
    //     // //         AVal(_, b) => Ok(AVal((), b.clone())),
    //     // //         And(bl, br) => Ok(And(
    //     // //             Box::new(self.decorate_anf(bl)?),
    //     // //             Box::new(self.decorate_anf(br)?),
    //     // //         )),
    //     // //         Or(bl, br) => Ok(Or(
    //     // //             Box::new(self.decorate_anf(bl)?),
    //     // //             Box::new(self.decorate_anf(br)?),
    //     // //         )),
    //     // //         Neg(bl) => Ok(Neg(Box::new(self.decorate_anf(bl)?))),
    //     // //     }
    //     // // }
    //     // // pub fn decorate_anfs(&mut self, anfs: &[AnfAnn]) -> Result<Vec<AnfAnlys>, CompileError> {
    //     // //     anfs.iter().map(|a| self.decorate_anf(a)).collect()
    //     // // }
    //     // // pub fn decorate_expr(&mut self, e: &ExprAnn) -> Result<ExprAnlys, CompileError> {
    //     // //     use crate::grammar::Expr::*;
    //     // //     match e {
    //     // //         EAnf(_, a) => Ok(EAnf((), Box::new(self.decorate_anf(a)?))),
    //     // //         EPrj(_, i, a) => Ok(EPrj((), *i, Box::new(self.decorate_anf(a)?))),
    //     // //         EFst(_, a) => Ok(EFst((), Box::new(self.decorate_anf(a)?))),
    //     // //         ESnd(_, a) => Ok(ESnd((), Box::new(self.decorate_anf(a)?))),
    //     // //         EProd(_, anfs) => Ok(EProd((), self.decorate_anfs(anfs)?)),
    //     // //         ELetIn(v, s, ebound, ebody) => match self.decor.get(v) {
    //     // //             None => panic!("impossible"),
    //     // //             Some(dv) => Ok(ELetIn(
    //     // //                 dv.clone(),
    //     // //                 s.clone(),
    //     // //                 Box::new(self.decorate_expr(ebound)?),
    //     // //                 Box::new(self.decorate_expr(ebody)?),
    //     // //             )),
    //     // //         },
    //     // //         EIte(_ty, cond, t, f) => Ok(EIte(
    //     // //             (),
    //     // //             Box::new(self.decorate_anf(cond)?),
    //     // //             Box::new(self.decorate_expr(t)?),
    //     // //             Box::new(self.decorate_expr(f)?),
    //     // //         )),

    //     // //         EFlip(v, param) => match self.decor.get(v) {
    //     // //             None => panic!("impossible"),
    //     // //             Some(dv) => Ok(EFlip(dv.clone(), *param)),
    //     // //         },
    //     // //         EObserve(_, a) => {
    //     // //             let anf = self.decorate_anf(a)?;
    //     // //             Ok(EObserve((), Box::new(anf)))
    //     // //         }
    //     // //         ESample(_, e) => Ok(ESample((), Box::new(self.decorate_expr(e)?))),
    //     // //     }
    //     // // }

    //     // // pub fn compile_decorations(&mut self) {
    //     // //     for (var, (above, below)) in self.above_below.clone() {
    //     // //         let dv = DecoratedVar::new(&var, above, below);
    //     // //         self.decor.insert(var, dv);
    //     // //     }
    //     // // }

    //     // // pub fn interaction_graph(&mut self) -> Result<(), CompileError> {
    //     // //     Ok(())
    //     // // }

    //     // // pub fn decorate(
    //     // //     &mut self,
    //     // //     p: &ProgramAnn,
    //     // //     analyze: bool,
    //     // // ) -> Result<(ProgramAnlys, Interaction), CompileError> {
    //     // //     match p {
    //     // //         Program::Body(e) => {
    //     // //             if analyze {
    //     // //                 self.plan_expr(e)?;
    //     // //                 self.compile_decorations();
    //     // //                 debug!("final values");
    //     // //                 for (k, (a, b)) in self.above_below.iter() {
    //     // //                     let above: HashSet<String> =
    //     // //                         a.iter().cloned().map(|v| v.debug_id()).collect();

    //     // //                     let below: HashSet<String> =
    //     // //                         b.iter().cloned().map(|v| v.debug_id()).collect();

    //     // //                     debug!("[{:?}]\t{:?}\t[{:?}]", above, &k.debug_id(), below);
    //     // //                 }
    //     // //             }
    //     // //             let fin = self.decorate_expr(e)?;
    //     // //             Ok((Program::Body(fin), self.above_below.clone()))
    //     // //         }
    //     // //     }
    //     // // }
    // }

    // // use crate::annotate::{InvMap, LabelEnv};
    // // use crate::compile::{compile, Mgr};
    // // use crate::typecheck::{
    // //     grammar::{ExprTyped, ProgramTyped},
    // //     typecheck,
    // // };
    // // use crate::uniquify::SymEnv;
    // // use crate::Options;
    // // pub fn compile_h(
    // //     p: &ProgramTyped,
    // //     mgr: &mut Mgr,
    // //     opt: &Options,
    // // ) -> Result<(ProgramAnn, Interaction), CompileError> {
    // //     let p = typecheck(p)?;
    // //     let mut senv = SymEnv::default();
    // //     let p = senv.uniquify(&p)?;
    // //     let mut lenv = LabelEnv::new();
    // //     let (p, vo, varmap, inv, mxlbl) = lenv.annotate(&p)?;

    // //     // let mut aenv = InteractionEnv::new(&varmap);
    // //     //aenv.decorate(&p, opt.opt)
    // //     todo!()
}

// #[cfg(test)]
// mod tests {
//     use crate::compile::*;
//     use crate::grammar::*;
//     use crate::grammar_macros::*;
//     use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
//     use crate::typecheck::typecheck;
//     use crate::*;
//     use tracing::*;
//     use tracing_test::traced_test;
// }
