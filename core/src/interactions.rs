use crate::annotate::grammar::*;
use crate::compile::CompileError;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::CompileError::Generic;
// use grammar::*;
use rsdd::builder::bdd_plan::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use rsdd::util::hypergraph::*;
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
    graph: Hypergraph<PlanPtr>,
}
impl PlanManager {
    fn fresh(&mut self) -> PlanPtr {
        let p = PlanPtr(self.gensym);
        self.gensym += 1;
        p
    }
    fn flatten(&self, ptr: PlanPtr) -> HashSet<PlanPtr> {
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
    fn flatten_all(&self, ps: &[PlanPtr]) -> HashSet<PlanPtr> {
        ps.into_iter().fold(HashSet::new(), |mut fin, p| {
            fin.extend(&self.flatten(*p));
            fin
        })
    }
    pub fn var(&mut self, v: VarLabel, polarity: bool) -> PlanPtr {
        let lit = PlanNode::Literal(v, polarity);
        let ptr = self.fresh();
        self.store.insert(ptr, lit);

        self.graph.insert_vertex(ptr);
        ptr
    }
    fn binop_h(
        &mut self,
        l: PlanPtr,
        r: PlanPtr,
        op: &dyn Fn(PlanPtr, PlanPtr) -> PlanNode,
    ) -> PlanPtr {
        self.graph.insert_edge(&self.flatten_all(&[l, r]));
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
        self.graph.insert_edge(&self.flatten_all(&[p, l, r]));
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
}

// pub mod grammar {
//     use super::*;
//     pub use crate::annotate::grammar::{BddVar, NamedVar, Var};
//     pub use crate::uniquify::grammar::UniqueId;
//     use std::fmt;
//     use std::fmt::*;

//     #[derive(Clone, Eq, PartialEq, Debug)]
//     pub struct Decorated<X> {
//         pub var: X,
//         pub above: HashSet<Var>,
//         pub below: HashSet<Var>,
//     }
//     pub type DecoratedNamedVar = Decorated<NamedVar>;
//     pub type DecoratedBddVar = Decorated<BddVar>;

//     #[derive(Clone, Eq, PartialEq, Debug)]
//     pub enum DecoratedVar {
//         Bdd(DecoratedBddVar),
//         Named(DecoratedNamedVar),
//     }
//     impl DecoratedVar {
//         pub fn new(v: &Var, above: HashSet<Var>, below: HashSet<Var>) -> Self {
//             match v {
//                 Var::Named(var) => DecoratedVar::Named(Decorated {
//                     above,
//                     below,
//                     var: var.clone(),
//                 }),
//                 Var::Bdd(var) => DecoratedVar::Bdd(Decorated {
//                     above,
//                     below,
//                     var: var.clone(),
//                 }),
//             }
//         }
//         pub fn id(&self) -> UniqueId {
//             self.var().id()
//         }
//         pub fn below(&self) -> &HashSet<Var> {
//             match self {
//                 DecoratedVar::Named(d) => &d.below,
//                 DecoratedVar::Bdd(d) => &d.below,
//             }
//         }
//         pub fn above(&self) -> &HashSet<Var> {
//             match self {
//                 DecoratedVar::Named(d) => &d.above,
//                 DecoratedVar::Bdd(d) => &d.above,
//             }
//         }
//         pub fn var(&self) -> Var {
//             match self {
//                 DecoratedVar::Named(d) => Var::Named(d.var.clone()),
//                 DecoratedVar::Bdd(d) => Var::Bdd(d.var.clone()),
//             }
//         }
//         pub fn debug_id(&self) -> String {
//             self.var().debug_id()
//         }
//         pub fn from_var(var: &Var) -> DecoratedVar {
//             DecoratedVar::new(var, HashSet::new(), HashSet::new())
//         }
//     }

//     #[derive(Debug, PartialEq, Clone)]
//     pub struct Interaction;

//     impl ξ<Interaction> for AVarExt {
//         type Ext = DecoratedVar;
//     }
//     impl ξ<Interaction> for AValExt {
//         type Ext = ();
//     }
//     pub type AnfAnlys = Anf<Interaction>;

//     impl ξ<Interaction> for EAnfExt {
//         type Ext = ();
//     }
//     impl ξ<Interaction> for EFstExt {
//         type Ext = ();
//     }
//     impl ξ<Interaction> for ESndExt {
//         type Ext = ();
//     }
//     impl ξ<Interaction> for EPrjExt {
//         type Ext = ();
//     }
//     impl ξ<Interaction> for EProdExt {
//         type Ext = ();
//     }
//     impl ξ<Interaction> for ELetInExt {
//         type Ext = ();
//     }
//     impl ξ<Interaction> for EIteExt {
//         type Ext = ();
//     }
//     impl ξ<Interaction> for EFlipExt {
//         type Ext = DecoratedBddVar;
//     }
//     impl ξ<Interaction> for EObserveExt {
//         type Ext = ();
//     }

//     #[derive(PartialEq, Copy, Clone, Debug, Hash, Eq)]
//     pub enum SamplingPosition {
//         Pos(UniqueId, usize),
//     }

//     impl ξ<Interaction> for ESampleExt {
//         type Ext = Vec<Decorated<SamplingPosition>>;
//     }

//     pub type ExprAnlys = Expr<Interaction>;
//     pub type ProgramAnlys = Program<Interaction>;
// }
// #[derive(PartialEq, Copy, Clone, Debug, Hash, Eq)]
// enum State {
//     Declaration,
//     Dependence,
//     Projection,
// }
// #[derive(Clone, Debug)]
// pub enum SamplingContext {
//     Unset,
//     Set(Var),
//     Sampling(Var),
//     SamplingWithLet(Var, Var),
// }
// impl SamplingContext {
//     pub fn as_option(&self) -> Option<Var> {
//         use SamplingContext::*;
//         match self {
//             Unset => None,
//             Set(v) | Sampling(v) => Some(v.clone()),
//             SamplingWithLet(v, _) => Some(v.clone()),
//         }
//     }
// }
// pub struct InteractionEnv {
//     gensym : usize,
// }

// pub struct AbstractBddPtr(usize);

// pub struct Ctx {
//     substitutions: HashMap<Var, Vec<BddPlan>>,
//     dependencies: HashMap<BddPlan, HashSet<Var>>,
// }

// type Deps = HashMap<AbstractBddPtr, HashSet<Var>>;

// impl InteractionEnv {
//     pub fn new() -> InteractionEnv {
//         Self {
//             gensym : 0,
//         }
//     }
//     pub fn fresh_ptr(&mut self) -> AbstractBddPtr {
//         let p = AbstractBddPtr(self.gensym);
//         self.gensym += 1;
//         p
//     }

//     pub fn analyze_anf(&mut self, ctx:&mut Ctx, a: &AnfAnn) -> Result<(), CompileError> {
//         use crate::grammar::Anf::*;
//         match a {
//             AVar(v, s) => match ctx.substitutions.get(&v) {
//                 None => panic!("must already be included"),
//                 Some(ptrs) => {
//                     // for p in ptrs {
//                     //     ctx.dependencies.get(p);
//                     // }
//                     todo!("what does this mean?")
//                 }
//             }
//             AVal(_, b) => Ok(()),
//             _ => todo!(),
//             // And(bl, br) => {
//             //     let al = self.analyze_anf(ctx, bl);
//             //     let ar = self.analyze_anf(ctx, br);
//             //     let p = self.fresh_ptr();

//             //     self.state = State::Dependence;
//             //     self.analyze_anf(bl)?;
//             //     self.analyze_anf(br)?;
//             //     Ok(())
//             // }
//             // Or(bl, br) => {
//             //     self.state = State::Dependence;
//             //     self.analyze_anf(bl)?;
//             //     self.analyze_anf(br)?;
//             //     Ok(())
//             // }
//             // Neg(bl) => {
//             //     self.state = State::Dependence;
//             //     self.analyze_anf(bl)
//             // }
//         }
//     }
//     // pub fn analyze_anfs(&mut self, anfs: &[AnfAnn]) -> Result<(), CompileError> {
//     //     for (i, a) in anfs.iter().enumerate() {
//     //         self.tuple_ix = i;
//     //         self.analyze_anf(a)?;
//     //     }
//     //     self.tuple_ix = 0;
//     //     Ok(())
//     // }
//     // fn state_bracket(
//     //     &mut self,
//     //     s: State,
//     //     f: &mut dyn FnMut(&mut Self) -> Result<(), CompileError>,
//     // ) -> Result<(), CompileError> {
//     //     let prv = self.state;
//     //     self.state = s;
//     //     let r = f(self);
//     //     self.state = prv;
//     //     r
//     // }
//     // pub fn analyze_expr(&mut self, e: &ExprAnn) -> Result<(), CompileError> {
//     //     use crate::grammar::Expr::*;
//     //     use State::*;
//     //     match e {
//     //         EAnf(_, a) => self.analyze_anf(a),
//     //         EPrj(_, i, a) => self.state_bracket(Projection, &mut |s| s.analyze_anf(a)),
//     //         EFst(_, a) => self.state_bracket(Projection, &mut |s| s.analyze_anf(a)),
//     //         ESnd(_, a) => self.state_bracket(Projection, &mut |s| s.analyze_anf(a)),
//     //         EProd(_, az) => self.state_bracket(Projection, &mut |s| s.analyze_anfs(az)),
//     //         ELetIn(v, s, ebound, ebody) => {
//     //             use SamplingContext::*;
//     //             self.state = Dependence;
//     //             match &self.sampling_context {
//     //                 Unset | Set(_) => {
//     //                     self.sampling_context = Set(v.clone());
//     //                 }
//     //                 Sampling(prv) => {
//     //                     self.sampling_context = SamplingWithLet(prv.clone(), v.clone());
//     //                 }
//     //                 SamplingWithLet(prv, _) => {
//     //                     self.sampling_context = SamplingWithLet(prv.clone(), v.clone());
//     //                 }
//     //             }
//     //             self.analyze_expr(ebound)?;
//     //             self.analyze_expr(ebody)?;
//     //             Ok(())
//     //         }
//     //         EIte(_ty, cond, t, f) => {
//     //             self.state = Dependence;
//     //             self.analyze_anf(cond)?;
//     //             self.analyze_expr(t)?;
//     //             self.analyze_expr(f)?;
//     //             Ok(())
//     //         }
//     //         EFlip(v, param) => Ok(()),
//     //         EObserve(_, a) => {
//     //             self.state = Dependence;
//     //             self.analyze_anf(a)
//     //         }
//     //         ESample(_, e) => {
//     //             use SamplingContext::*;

//     //             match &self.sampling_context {
//     //                 Sampling(_) | Unset => {}
//     //                 Set(v) => {
//     //                     self.sampling_context = Sampling(v.clone());
//     //                 }
//     //                 SamplingWithLet(_, v) => {
//     //                     self.sampling_context = Sampling(v.clone());
//     //                 }
//     //             }

//     //             self.analyze_expr(e)
//     //         }
//     //     }
//     // }
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
//     // //                 self.analyze_expr(e)?;
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
// // }

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
