#![allow(non_camel_case_types)]

/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
use crate::*;
use core::fmt;
use core::fmt::Debug;
use itertools::Itertools;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::string::String;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Bool,
    Prod(Vec<Ty>),
}
impl Ty {
    pub fn right(&self) -> Option<Ty> {
        match self {
            Ty::Bool => None,
            Ty::Prod(tys) => match &tys[..] {
                [_, r] => Some(r.clone()),
                _ => None,
            },
        }
    }
    pub fn left(&self) -> Option<Ty> {
        match self {
            Ty::Bool => None,
            Ty::Prod(tys) => match &tys[..] {
                [l, _] => Some(l.clone()),
                _ => None,
            },
        }
    }
}

pub trait ξ<X> {
    type Ext;
}
pub struct EAnfExt;
pub struct EPrjExt;
pub struct EProdExt;
pub struct ELetInExt;
pub struct EIteExt;
pub struct EFlipExt;
pub struct EObserveExt;
pub struct ESampleExt;
// fucking ridicuous
pub struct AVarExt;
pub struct AValExt;

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Bool(bool),
    Prod(Vec<Val>),
}
impl Val {
    pub fn is_prod(&self) -> bool {
        use Val::*;
        match self {
            Bool(_) => false,
            Prod(_) => true,
        }
    }
    pub fn as_type(&self) -> Ty {
        use Val::*;
        match self {
            Bool(_) => Ty::Bool,
            Prod(vs) => Ty::Prod(vs.iter().map(|x| x.as_type()).collect_vec()),
        }
    }
}

pub enum Anf<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
{
    AVar(<AVarExt as ξ<X>>::Ext, String),
    AVal(<AValExt as ξ<X>>::Ext, Val),

    // TODO: not sure this is where I should add booleans, but it makes
    // the observe statements stay closer to the semantics: ~observe anf~
    And(Box<Anf<X>>, Box<Anf<X>>),
    Or(Box<Anf<X>>, Box<Anf<X>>),
    Neg(Box<Anf<X>>),
}

impl<X> Debug for Anf<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Debug,
    <AValExt as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Anf::*;
        match self {
            AVar(ext, s) => f.write_fmt(format_args!("Var({:?}) {}", ext, s)),
            AVal(ext, v) => f.write_fmt(format_args!("Val({:?}) {:?}", ext, v)),
            And(l, r) => f.write_fmt(format_args!("And({:?} && {:?})", l, r)),
            Or(l, r) => f.write_fmt(format_args!("Or({:?} || {:?})", l, r)),
            Neg(n) => f.write_fmt(format_args!("!({:?})", n)),
        }
    }
}
impl<X> Clone for Anf<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Clone,
    <AValExt as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use Anf::*;
        match self {
            AVar(ext, x) => AVar(ext.clone(), x.clone()),
            AVal(ext, x) => AVal(ext.clone(), x.clone()),
            And(l, r) => And(l.clone(), r.clone()),
            Or(l, r) => Or(l.clone(), r.clone()),
            Neg(x) => Neg(x.clone()),
        }
    }
}
impl<X> PartialEq for Anf<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: PartialEq,
    <AValExt as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use Anf::*;
        match (self, o) {
            (AVar(ext0, a0), AVar(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (AVal(ext0, a0), AVal(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (And(ext0, a0), And(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (Or(ext0, a0), Or(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (Neg(a0), Neg(a1)) => a0 == a1,
            (_, _) => false,
        }
    }
}

#[allow(clippy::enum_variant_names)]
pub enum Expr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
{
    EAnf(<EAnfExt as ξ<X>>::Ext, Box<Anf<X>>),

    EPrj(<EPrjExt as ξ<X>>::Ext, usize, Box<Anf<X>>),
    EProd(<EProdExt as ξ<X>>::Ext, Vec<Anf<X>>),

    // TODO Ignore function calls for now
    // EApp(String, Box<Anf>),
    ELetIn(<ELetInExt as ξ<X>>::Ext, String, Box<Expr<X>>, Box<Expr<X>>),
    EIte(
        <EIteExt as ξ<X>>::Ext,
        Box<Anf<X>>,
        Box<Expr<X>>,
        Box<Expr<X>>,
    ),
    EFlip(<EFlipExt as ξ<X>>::Ext, f64),
    EObserve(<EObserveExt as ξ<X>>::Ext, Box<Anf<X>>),
    ESample(<ESampleExt as ξ<X>>::Ext, Box<Expr<X>>),
}
impl<X> Debug for Expr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug,
    <EPrjExt as ξ<X>>::Ext: Debug,
    <EProdExt as ξ<X>>::Ext: Debug,
    <ELetInExt as ξ<X>>::Ext: Debug,
    <EIteExt as ξ<X>>::Ext: Debug,
    <EFlipExt as ξ<X>>::Ext: Debug,
    <EObserveExt as ξ<X>>::Ext: Debug,
    <ESampleExt as ξ<X>>::Ext: Debug,
    <AVarExt as ξ<X>>::Ext: Debug,
    <AValExt as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            EAnf(ext, a) => f.write_fmt(format_args!("Anf({:?},{:?})", ext, a)),
            EPrj(ext, i, a) => f.debug_tuple("Prj").field(&ext).field(i).field(a).finish(),
            EProd(ext, anfs) => f.debug_tuple("Prod").field(&ext).field(anfs).finish(),
            ELetIn(ext, s, bindee, body) => f
                .debug_struct("LetIn")
                .field("ext", &ext)
                .field("var", s)
                .field("bindee", bindee)
                .field("body", body)
                .finish(),
            EIte(ext, p, tru, fls) => f
                .debug_struct("Ite")
                .field("ext", &ext)
                .field("predicate", p)
                .field("truthy", tru)
                .field("falsey", fls)
                .finish(),
            EFlip(ext, p) => f.write_fmt(format_args!("Flip({:?}, {:?})", ext, p)),
            EObserve(ext, a) => f.debug_tuple("Observe").field(&ext).field(a).finish(),
            ESample(ext, e) => f.debug_tuple("Sample").field(&ext).field(e).finish(),
        }
    }
}
impl<X> Clone for Expr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,

    <EAnfExt as ξ<X>>::Ext: Clone,
    <EPrjExt as ξ<X>>::Ext: Clone,
    <EProdExt as ξ<X>>::Ext: Clone,
    <ELetInExt as ξ<X>>::Ext: Clone,
    <EIteExt as ξ<X>>::Ext: Clone,
    <EFlipExt as ξ<X>>::Ext: Clone,
    <EObserveExt as ξ<X>>::Ext: Clone,
    <ESampleExt as ξ<X>>::Ext: Clone,
    <AVarExt as ξ<X>>::Ext: Clone,
    <AValExt as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use Expr::*;
        match self {
            EAnf(ext, a) => EAnf(ext.clone(), a.clone()),
            EPrj(ext, i, a) => EPrj(ext.clone(), *i, a.clone()),
            EProd(ext, anfs) => EProd(ext.clone(), anfs.clone()),
            ELetIn(ext, s, bindee, body) => {
                ELetIn(ext.clone(), s.clone(), bindee.clone(), body.clone())
            }
            EIte(ext, p, tru, fls) => EIte(ext.clone(), p.clone(), tru.clone(), fls.clone()),
            EFlip(ext, p) => EFlip(ext.clone(), *p),
            EObserve(ext, a) => EObserve(ext.clone(), a.clone()),
            ESample(ext, e) => ESample(ext.clone(), e.clone()),
        }
    }
}
impl<X> PartialEq for Expr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,

    <EAnfExt as ξ<X>>::Ext: PartialEq,
    <EPrjExt as ξ<X>>::Ext: PartialEq,
    <EProdExt as ξ<X>>::Ext: PartialEq,
    <ELetInExt as ξ<X>>::Ext: PartialEq,
    <EIteExt as ξ<X>>::Ext: PartialEq,
    <EFlipExt as ξ<X>>::Ext: PartialEq,
    <EObserveExt as ξ<X>>::Ext: PartialEq,
    <ESampleExt as ξ<X>>::Ext: PartialEq,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: PartialEq,
    <AValExt as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use Expr::*;
        match (self, o) {
            (EAnf(ext0, a0), EAnf(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (EPrj(ext0, i0, a0), EPrj(ext1, i1, a1)) => ext0 == ext1 && i0 == i1 && a0 == a1,
            (EProd(ext0, anfs0), EProd(ext1, anfs1)) => ext0 == ext1 && anfs0 == anfs1,
            (ELetIn(ext0, s0, bindee0, body0), ELetIn(ext1, s1, bindee1, body1)) => {
                ext0 == ext1 && s0 == s1 && bindee0 == bindee1 && body0 == body1
            }
            (EIte(ext0, p0, tru0, fls0), EIte(ext1, p1, tru1, fls1)) => {
                ext0 == ext1 && p0 == p1 && tru0 == tru1 && fls0 == fls1
            }
            (EFlip(ext0, p0), EFlip(ext1, p1)) => ext0 == ext1 && p0 == p1,
            (EObserve(ext0, a0), EObserve(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (ESample(ext0, e0), ESample(ext1, e1)) => ext0 == ext1 && e0 == e1,
            (_, _) => false,
        }
    }
}
#[derive(Default, Debug, Clone)]
pub struct Γ(pub Vec<(String, Ty)>);
impl Γ {
    pub fn get(&self, x: String) -> Option<Ty> {
        self.0
            .iter()
            .find(|(e, _)| *e == x)
            .map(|(_, t)| t)
            .cloned()
    }
    pub fn push(&mut self, x: String, ty: &Ty) {
        self.0.push((x, ty.clone()));
    }
    pub fn append(&self, x: String, ty: &Ty) -> Γ {
        let mut ctx = self.0.clone();
        ctx.push((x, ty.clone()));
        Γ(ctx)
    }
    pub fn typechecks(&self, x: String, ty: &Ty) -> bool {
        self.get(x).map(|t| t == *ty).is_some()
    }
}
// TODO
// structure Func where
//   name : String
//   arg : String × Ty
//   ret : Ty
//   body : Expr
// deriving Repr

impl<X> Expr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone,
    <EProdExt as ξ<X>>::Ext: Debug + Clone,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone,
    <EIteExt as ξ<X>>::Ext: Debug + Clone,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Debug + Clone,
    <AValExt as ξ<X>>::Ext: Debug + Clone,
{
    pub fn is_sample(&self) -> bool {
        matches!(self, Expr::ESample(_, _))
    }
    pub fn strip_samples1(&self) -> Expr<X> {
        use Expr::*;
        match self {
            ESample(_, e) => e.strip_samples1(),
            ELetIn(ex, s, x, y) => ELetIn(
                ex.clone(),
                s.clone(),
                Box::new(x.strip_samples1()),
                Box::new(y.strip_samples1()),
            ),
            EIte(ex, p, x, y) => EIte(
                ex.clone(),
                p.clone(),
                Box::new(x.strip_samples1()),
                Box::new(y.strip_samples1()),
            ),
            e => e.clone(),
        }
    }
    pub fn query(&self) -> Expr<X> {
        use Expr::*;
        match self {
            ELetIn(ex, s, x, y) => y.query(),
            _ => self.clone(),
        }
    }
    pub fn insert_observe(&self, e: Expr<X>) -> Expr<X> {
        use Expr::*;
        match self {
            ELetIn(ex, s, x, body) => {
                let body = match &**body {
                    ELetIn(_, _, _, _) => body.insert_observe(e),
                    _ => ELetIn(
                        ex.clone(),
                        "_".to_string(),
                        Box::new(e.clone()),
                        body.clone(),
                    ),
                };
                ELetIn(
                    ex.clone(),
                    s.to_string(),
                    Box::new(*x.clone()),
                    Box::new(body),
                )
            }
            _ => self.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Program<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EProdExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EIteExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <AValExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
{
    Body(Expr<X>),
    // TODO
    // | define (f: Func) (rest: Program) : Program
}
impl<X> Program<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EProdExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EIteExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <AValExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
{
    pub fn strip_samples(&self) -> Program<X> {
        use Program::*;
        match self {
            Body(e) => {
                // FIXME: this shouldn't be necessary and I think I already fixed the bug that causes this.
                let mut cur = e.strip_samples1();
                loop {
                    let nxt = cur.strip_samples1();

                    if nxt == cur {
                        return Body(nxt);
                    } else {
                        cur = nxt;
                    }
                }
            }
        }
    }
    pub fn query(&self) -> Expr<X> {
        use Program::*;
        match self {
            Body(e) => e.query(),
        }
    }
    pub fn insert_observe(&self, e: Expr<X>) -> Program<X> {
        use Program::*;
        match self {
            Body(b) => Body(b.insert_observe(e)),
        }
    }
}

// UD comprises of the default, undecorated grammar. Grammar.rs seems to be the best place for this.
pub struct UD;
impl ξ<UD> for AVarExt {
    type Ext = ();
}
impl ξ<UD> for AValExt {
    type Ext = ();
}
impl ξ<UD> for EAnfExt {
    type Ext = ();
}
impl ξ<UD> for EPrjExt {
    type Ext = ();
}
impl ξ<UD> for EProdExt {
    type Ext = ();
}
impl ξ<UD> for ELetInExt {
    type Ext = ();
}
impl ξ<UD> for EIteExt {
    type Ext = ();
}
impl ξ<UD> for EFlipExt {
    type Ext = ();
}
impl ξ<UD> for EObserveExt {
    type Ext = ();
}
impl ξ<UD> for ESampleExt {
    type Ext = ();
}

pub type AnfUD = Anf<UD>;
pub type ExprUD = Expr<UD>;
pub type ProgramUD = Program<UD>;
