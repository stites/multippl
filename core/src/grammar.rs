#![allow(dead_code)]
#![allow(non_camel_case_types)]
use crate::*;
use core::fmt;
use core::fmt::Debug;
use itertools::Itertools;
use quickcheck::*;
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

#[derive(Debug, Clone, PartialEq)]
pub enum ANF {
    AVar(String, Box<Ty>), // FIXME
    AVal(Val),

    // TODO: not sure this is where I should add booleans, but it makes
    // the observe statements stay closer to the semantics: ~observe anf~
    And(Box<ANF>, Box<ANF>),
    Or(Box<ANF>, Box<ANF>),
    Neg(Box<ANF>),
}
impl ANF {
    pub fn as_type(&self) -> Ty {
        use ANF::*;
        match self {
            AVar(_, t) => *t.clone(),
            AVal(v) => v.as_type(),
            _ => Ty::Bool,
        }
    }
    pub fn is_type(&self, ty: &Ty) -> bool {
        self.as_type() == *ty
    }
}

pub trait ξ<X> {
    type Ext;
}
pub struct EAnfExt;
pub struct EFstExt;
pub struct ESndExt;
pub struct EPrjExt;
pub struct EProdExt;
pub struct ELetInExt;
pub struct EIteExt;
pub struct EFlipExt;
pub struct EObserveExt;
pub struct ESampleExt;

pub struct UD;
impl ξ<UD> for EAnfExt {
    type Ext = ();
}
impl ξ<UD> for EFstExt {
    type Ext = ();
}
impl ξ<UD> for ESndExt {
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

pub type ExprUD = Expr<UD>;

// #[derive(Debug, Clone, PartialEq)]
pub enum Expr<X>
where
    EAnfExt: ξ<X>,
    EFstExt: ξ<X>,
    ESndExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
{
    EAnf(<EAnfExt as ξ<X>>::Ext, Box<ANF>),

    EFst(<EFstExt as ξ<X>>::Ext, Box<ANF>, Box<Ty>),
    ESnd(<ESndExt as ξ<X>>::Ext, Box<ANF>, Box<Ty>),
    EPrj(<EPrjExt as ξ<X>>::Ext, usize, Box<ANF>, Box<Ty>),
    EProd(<EProdExt as ξ<X>>::Ext, Vec<ANF>, Box<Ty>),

    // TODO Ignore function calls for now
    // EApp(String, Box<ANF>),
    ELetIn(
        <ELetInExt as ξ<X>>::Ext,
        String,
        Box<Ty>,
        Box<Expr<X>>,
        Box<Expr<X>>,
        Box<Ty>,
    ),
    EIte(
        <EIteExt as ξ<X>>::Ext,
        Box<ANF>,
        Box<Expr<X>>,
        Box<Expr<X>>,
        Box<Ty>,
    ),
    EFlip(<EFlipExt as ξ<X>>::Ext, f64),
    EObserve(<EObserveExt as ξ<X>>::Ext, Box<ANF>),
    ESample(<ESampleExt as ξ<X>>::Ext, Box<Expr<X>>),
}
impl<X> Debug for Expr<X>
where
    EAnfExt: ξ<X>,
    EFstExt: ξ<X>,
    ESndExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug,
    <EFstExt as ξ<X>>::Ext: Debug,
    <ESndExt as ξ<X>>::Ext: Debug,
    <EPrjExt as ξ<X>>::Ext: Debug,
    <EProdExt as ξ<X>>::Ext: Debug,
    <ELetInExt as ξ<X>>::Ext: Debug,
    <EIteExt as ξ<X>>::Ext: Debug,
    <EFlipExt as ξ<X>>::Ext: Debug,
    <EObserveExt as ξ<X>>::Ext: Debug,
    <ESampleExt as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            EAnf(ext, a) => f
                .debug_struct("Anf")
                .field("ext", &ext)
                .field("anf", a)
                .finish(),
            EFst(ext, a, t) => f
                .debug_struct("Fst")
                .field("ext", &ext)
                .field("anf", a)
                .field("type", t)
                .finish(),
            ESnd(ext, a, t) => f
                .debug_struct("Snd")
                .field("ext", &ext)
                .field("anf", a)
                .field("type", t)
                .finish(),
            EPrj(ext, i, a, t) => f
                .debug_struct("Prj")
                .field("ext", &ext)
                .field("ix", i)
                .field("anf", a)
                .field("type", t)
                .finish(),
            EProd(ext, anfs, t) => f
                .debug_struct("Prod")
                .field("ext", &ext)
                .field("prod", anfs)
                .field("type", t)
                .finish(),
            ELetIn(ext, s, ty1, bindee, body, ty2) => f
                .debug_struct("LetIn")
                .field("ext", &ext)
                .field("var", s)
                .field("var-type", ty1)
                .field("bindee", bindee)
                .field("body", body)
                .field("body-type", ty2)
                .finish(),
            EIte(ext, p, tru, fls, ty) => f
                .debug_struct("Ite")
                .field("ext", &ext)
                .field("predicate", p)
                .field("truthy", tru)
                .field("falsey", fls)
                .field("type", ty)
                .finish(),
            EFlip(ext, p) => f
                .debug_struct("Flip")
                .field("ext", &ext)
                .field("param", p)
                .finish(),
            EObserve(ext, a) => f
                .debug_struct("Observe")
                .field("ext", &ext)
                .field("anf", a)
                .finish(),
            ESample(ext, e) => f
                .debug_struct("Sample")
                .field("ext", &ext)
                .field("subprogram", e)
                .finish(),
        }
    }
}
impl<X> Clone for Expr<X>
where
    EAnfExt: ξ<X>,
    EFstExt: ξ<X>,
    ESndExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Clone,
    <EFstExt as ξ<X>>::Ext: Clone,
    <ESndExt as ξ<X>>::Ext: Clone,
    <EPrjExt as ξ<X>>::Ext: Clone,
    <EProdExt as ξ<X>>::Ext: Clone,
    <ELetInExt as ξ<X>>::Ext: Clone,
    <EIteExt as ξ<X>>::Ext: Clone,
    <EFlipExt as ξ<X>>::Ext: Clone,
    <EObserveExt as ξ<X>>::Ext: Clone,
    <ESampleExt as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use Expr::*;
        match self {
            EAnf(ext, a) => EAnf(ext.clone(), a.clone()),
            EFst(ext, a, t) => EFst(ext.clone(), a.clone(), t.clone()),
            ESnd(ext, a, t) => ESnd(ext.clone(), a.clone(), t.clone()),
            EPrj(ext, i, a, t) => EPrj(ext.clone(), i.clone(), a.clone(), t.clone()),
            EProd(ext, anfs, t) => EProd(ext.clone(), anfs.clone(), t.clone()),
            ELetIn(ext, s, ty1, bindee, body, ty2) => ELetIn(
                ext.clone(),
                s.clone(),
                ty1.clone(),
                bindee.clone(),
                body.clone(),
                ty2.clone(),
            ),
            EIte(ext, p, tru, fls, ty) => {
                EIte(ext.clone(), p.clone(), tru.clone(), fls.clone(), ty.clone())
            }
            EFlip(ext, p) => EFlip(ext.clone(), p.clone()),
            EObserve(ext, a) => EObserve(ext.clone(), a.clone()),
            ESample(ext, e) => ESample(ext.clone(), e.clone()),
        }
    }
}
impl<X> PartialEq for Expr<X>
where
    EAnfExt: ξ<X>,
    EFstExt: ξ<X>,
    ESndExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: PartialEq,
    <EFstExt as ξ<X>>::Ext: PartialEq,
    <ESndExt as ξ<X>>::Ext: PartialEq,
    <EPrjExt as ξ<X>>::Ext: PartialEq,
    <EProdExt as ξ<X>>::Ext: PartialEq,
    <ELetInExt as ξ<X>>::Ext: PartialEq,
    <EIteExt as ξ<X>>::Ext: PartialEq,
    <EFlipExt as ξ<X>>::Ext: PartialEq,
    <EObserveExt as ξ<X>>::Ext: PartialEq,
    <ESampleExt as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use Expr::*;
        match (self, o) {
            (EAnf(ext0, a0), EAnf(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (EFst(ext0, a0, t0), EFst(ext1, a1, t1)) => ext0 == ext1 && a0 == a1 && t0 == t1,
            (ESnd(ext0, a0, t0), ESnd(ext1, a1, t1)) => ext0 == ext1 && a0 == a1 && t0 == t1,
            (EPrj(ext0, i0, a0, t0), EPrj(ext1, i1, a1, t1)) => {
                ext0 == ext1 && i0 == i1 && a0 == a1 && t0 == t1
            }
            (EProd(ext0, anfs0, t0), EProd(ext1, anfs1, t1)) => {
                ext0 == ext1 && anfs0 == anfs1 && t0 == t1
            }
            (
                ELetIn(ext0, s0, ty10, bindee0, body0, ty20),
                ELetIn(ext1, s1, ty11, bindee1, body1, ty21),
            ) => {
                ext0 == ext1
                    && s0 == s1
                    && ty10 == ty11
                    && bindee0 == bindee1
                    && body0 == body1
                    && ty20 == ty21
            }
            (EIte(ext0, p0, tru0, fls0, ty0), EIte(ext1, p1, tru1, fls1, ty1)) => {
                ext0 == ext1 && p0 == p1 && tru0 == tru1 && fls0 == fls1 && ty0 == ty1
            }
            (EFlip(ext0, p0), EFlip(ext1, p1)) => ext0 == ext1 && p0 == p1,
            (EObserve(ext0, a0), EObserve(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (ESample(ext0, e0), ESample(ext1, e1)) => ext0 == ext1 && e0 == e1,
            (_, _) => false,
        }
    }
}

impl Arbitrary for Val {
    fn arbitrary(g: &mut Gen) -> Self {
        let x = g.choose(&[None, Some(true), Some(false), Some(true), Some(false)]);
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(Some(b)) => Val::Bool(*b),
            Some(None) => {
                // let arb = u8::arbitrary(g);
                // let len = (arb % 3) + 2; // only generate between 2-4 tuples
                // Val::Prod((0..len).map(|_i| Self::arbitrary(g)).collect_vec())
                // ...actually, just work with 2-tuples for now.
                Val::Prod((0..2).map(|_i| Self::arbitrary(g)).collect_vec())
            }
        }
    }
}

impl Arbitrary for ANF {
    fn arbitrary(g: &mut Gen) -> Self {
        let x = g.choose(&[0, 1, 2_u8]).copied();
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(0) => ANF::AVar(String::arbitrary(g), Box::new(Ty::Bool)),
            Some(1) => ANF::AVal(Val::arbitrary(g)),
            Some(2) => {
                let x = g.choose(&[0, 1, 2_u8]);
                match x {
                    None => panic!("impossible: choose vec has len > 0"),
                    Some(0) => ANF::And(Box::<ANF>::arbitrary(g), Box::<ANF>::arbitrary(g)),
                    Some(1) => ANF::Or(Box::<ANF>::arbitrary(g), Box::<ANF>::arbitrary(g)),
                    Some(2) => ANF::Neg(Box::<ANF>::arbitrary(g)),
                    _ => panic!("impossible"),
                }
            }
            _ => panic!("impossible"),
        }
    }
}

impl<X: 'static> Arbitrary for Expr<X>
where
    EAnfExt: ξ<X>,
    EFstExt: ξ<X>,
    ESndExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Arbitrary,
    <EFstExt as ξ<X>>::Ext: Arbitrary,
    <ESndExt as ξ<X>>::Ext: Arbitrary,
    <EPrjExt as ξ<X>>::Ext: Arbitrary,
    <EProdExt as ξ<X>>::Ext: Arbitrary,
    <ELetInExt as ξ<X>>::Ext: Arbitrary,
    <EIteExt as ξ<X>>::Ext: Arbitrary,
    <EFlipExt as ξ<X>>::Ext: Arbitrary,
    <EObserveExt as ξ<X>>::Ext: Arbitrary,
    <ESampleExt as ξ<X>>::Ext: Arbitrary,
{
    fn arbitrary(g: &mut Gen) -> Expr<X> {
        let x = g.choose(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]).copied();
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(0) => Expr::EAnf(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            Some(1) => Expr::EFst(
                Arbitrary::arbitrary(g),
                Arbitrary::arbitrary(g),
                Box::new(Ty::Bool),
            ),
            Some(2) => Expr::ESnd(
                Arbitrary::arbitrary(g),
                Arbitrary::arbitrary(g),
                Box::new(Ty::Bool),
            ),
            Some(3) => Expr::EProd(
                Arbitrary::arbitrary(g),
                (0..2).map(|_i| ANF::arbitrary(g)).collect_vec(),
                Box::new(Ty::Bool),
            ),
            Some(4) => {
                let var = String::arbitrary(g);
                let bind = Arbitrary::arbitrary(g);
                let body = Arbitrary::arbitrary(g);
                Expr::ELetIn(
                    Arbitrary::arbitrary(g),
                    var,
                    Box::new(Ty::Bool),
                    bind,
                    body,
                    Box::new(Ty::Bool),
                )
            }
            Some(5) => {
                let p = Arbitrary::arbitrary(g);
                let t = Arbitrary::arbitrary(g);
                let f = Arbitrary::arbitrary(g);
                Expr::EIte(Arbitrary::arbitrary(g), p, t, f, Box::new(Ty::Bool))
            }
            Some(6) => {
                let r = u8::arbitrary(g); // 256
                Expr::EFlip(
                    Arbitrary::arbitrary(g),
                    <u8 as Into<f64>>::into(r) / <u8 as Into<f64>>::into(u8::MAX),
                )
            }
            Some(7) => Expr::EObserve(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            Some(8) => Expr::ESample(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            _ => panic!("impossible"),
        }
    }
}
// impl Arbitrary for Program {
//     fn arbitrary(g: &mut Gen) -> Self {
//         Program::Body(Expr::arbitrary(g))
//     }
// }

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
        self.0.push((x.clone(), ty.clone()));
    }
    pub fn append(&self, x: String, ty: &Ty) -> Γ {
        let mut ctx = self.0.clone();
        ctx.push((x.clone(), ty.clone()));
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
    EFstExt: ξ<X>,
    ESndExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone,
    <EFstExt as ξ<X>>::Ext: Debug + Clone,
    <ESndExt as ξ<X>>::Ext: Debug + Clone,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone,
    <EProdExt as ξ<X>>::Ext: Debug + Clone,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone,
    <EIteExt as ξ<X>>::Ext: Debug + Clone,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone,
{
    pub fn is_sample(&self) -> bool {
        use Expr::*;
        match self {
            ESample(_, _) => true,
            _ => false,
        }
    }
    pub fn is_type(&self, ty: &Ty) -> bool {
        self.as_type() == *ty
    }
    pub fn as_type(&self) -> Ty {
        use Expr::*;
        match self {
            EAnf(_, anf) => anf.as_type(),
            EFst(_, _, t) => *t.clone(),
            ESnd(_, _, t) => *t.clone(),
            EPrj(_, _, _, t) => *t.clone(),
            EProd(_, _, t) => *t.clone(),
            ELetIn(_, _, _, _, _, t) => *t.clone(),
            EIte(_, _, _, _, t) => *t.clone(),
            EFlip(_, _) => Ty::Bool,
            EObserve(_, _) => Ty::Bool,
            ESample(_, _) => Ty::Bool,
        }
    }

    pub fn strip_samples1(&self) -> Expr<X> {
        use Expr::*;
        match self {
            ESample(_, e) => e.strip_samples1(),
            ELetIn(ex, s, tx, x, y, ty) => ELetIn(
                ex.clone(),
                s.clone(),
                tx.clone(),
                Box::new(x.strip_samples1()),
                Box::new(y.strip_samples1()),
                ty.clone(),
            ),
            EIte(ex, p, x, y, ty) => EIte(
                ex.clone(),
                p.clone(),
                Box::new(x.strip_samples1()),
                Box::new(y.strip_samples1()),
                ty.clone(),
            ),
            e => e.clone(),
        }
    }
}

pub type ProgramUD = Program<UD>;
#[derive(Debug, Clone)]
pub enum Program<X>
where
    EAnfExt: ξ<X>,
    EFstExt: ξ<X>,
    ESndExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone,
    <EFstExt as ξ<X>>::Ext: Debug + Clone,
    <ESndExt as ξ<X>>::Ext: Debug + Clone,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone,
    <EProdExt as ξ<X>>::Ext: Debug + Clone,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone,
    <EIteExt as ξ<X>>::Ext: Debug + Clone,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone,
{
    Body(Expr<X>),
    // TODO
    // | define (f: Func) (rest: Program) : Program
}
impl<X> Program<X>
where
    EAnfExt: ξ<X>,
    EFstExt: ξ<X>,
    ESndExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EFstExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ESndExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EProdExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EIteExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
{
    pub fn strip_samples(&self) -> Program<X> {
        use Program::*;
        match self {
            Body(e) => {
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
}

// macros
#[macro_export]
macro_rules! typ {
    ($ty:ty) => {{
        if TypeId::of::<$ty>() == TypeId::of::<bool>() {
            Ty::Bool
        } else {
            todo!()
        }
    }};
    ($ty:expr) => {{
        if $ty == TypeId::of::<bool>() {
            Ty::Bool
        } else {
            todo!()
        }
    }};
}
#[macro_export]
macro_rules! B {
    () => {{
        Ty::Bool
    }};
}
#[macro_export]
macro_rules! P {
    ( $l:expr , $r:expr ) => {{
        Ty::Prod(Box::new($l), Box::new($r))
    }};
}

#[macro_export]
macro_rules! anf {
    ( $x:expr ) => {{
        Expr::EAnf((), Box::new($x))
    }};
}

#[macro_export]
macro_rules! val {
    ( $x:ident ) => {
        anf!(ANF::AVal(Val::Bool($x)))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = Box::new(Val::Bool($y));
        $(
            fin = Box::new(ANF::Prod(fin, Box::new(Val::Bool($x))));
        )+
        anf!(ANF::AVal(*fin))
    }};
}

#[macro_export]
macro_rules! var {
    ( $x:literal ) => {
        anf!(ANF::AVar($x.to_string(), Box::new(B!())))
    };
    ( $y:literal, $( $x:literal ),+ ) => {{
        let mut fin = anf!(ANF::AVar($y.to_string()));
        $(
            fin = Box::new(Expr::EProd((), fin, Box::new(anf!(ANF::AVar($x.to_string(), Box::new(B!()))))));
        )+
       *fin
    }};
}

#[macro_export]
macro_rules! b {
    (B) => {
        Ty::Bool
    };
    ( ) => {
        Ty::Bool
    };
    (@anf $x:literal ; $ty:expr) => {
        if $x.to_string() == "true" || $x.to_string() == "false" {
            ANF::AVal(Val::Bool($x.to_string() == "true"))
        } else {
            ANF::AVar($x.to_string(), Box::new($ty))
        }
    };
    (@anf $x:literal) => {
        b!(@anf $x ; B!())
    };
    ( true ) => { anf!(b!(@anf "true")) };
    ( false ) => { anf!(b!(@anf "false")) };
    ( $x:literal ) => {
        anf!(b!(@anf $x))
    };
    ( $x:literal ; $ty:expr ) => {
        anf!(b!(@anf $x ; $ty))
    };
    ( B , B ) => {{
        Ty::Prod(vec![b!(), b!()])
    }};
    ( B , B, B ) => {{
        Ty::Prod(vec![b!(), b!(), b!()])
    }};
    ( $x:literal, $( $xs:literal ),+  ) => {{
        let mut prod = vec![b!(@anf $x)];
        let mut typ = vec![b!()];
        $(
            prod.push(b!(@anf $xs));
            typ.push(b!());
        )+
        Expr::EProd((), prod, Box::new(Ty::Prod(typ)))
    }};
    ( $x:expr, $y:expr ) => {{
        let ty = Ty::Prod(vec![b!(), b!()]);
        Expr::EProd(vec![$x, $y], Box::new(ty))
    }};
    ( $y:literal && $( $x:literal )&&+ ) => {{
        let mut fin = Box::new(ANF::AVar($y.to_string(), Box::new(B!())));
        $(
            fin = Box::new(ANF::And(fin, Box::new(ANF::AVar($x.to_string(), Box::new(B!())))));
        )+
        anf!(*fin)
    }};
    ( ($y:expr) && $( ($x:expr) )&&+ ) => {{
        let mut fin = Box::new($y);
        $(
            fin = Box::new(ANF::And(fin, Box::new($x)));
        )+
        *fin
    }};
    ( $y:literal || $( $x:literal )||+ ) => {{
        let mut fin = Box::new(ANF::AVar($y.to_string(), Box::new(B!())));
        $(
            fin = Box::new(ANF::Or(fin, Box::new(ANF::AVar($x.to_string(), Box::new(B!())))));
        )+
        anf!(*fin)
    }};
    (@anf $y:literal && $( $x:literal )&&+ ) => {{
        let mut fin = Box::new(ANF::AVar($y.to_string(), Box::new(B!())));
        $(
            fin = Box::new(ANF::And(fin, Box::new(ANF::AVar($x.to_string(), Box::new(B!())))));
        )+
        *fin
    }};
    (@anf $y:literal || $( $x:literal )||+ ) => {{
        let mut fin = Box::new(ANF::AVar($y.to_string(), Box::new(B!())));
        $(
            fin = Box::new(ANF::Or(fin, Box::new(ANF::AVar($x.to_string(), Box::new(B!())))));
        )+
        *fin
    }};
}

#[macro_export]
macro_rules! q {
    ( $x:literal x $y:literal ) => {{
        let typ = vec![b!(); 4];
        let prod = vec![b!(@anf $x), b!(@anf $y), b!(@anf $x || $y), b!(@anf $x && $y)];
        Expr::EProd((), prod, Box::new(Ty::Prod(typ)))
    }};
}

#[macro_export]
macro_rules! snd {
    ( $x:literal ) => {{
        snd!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        Expr::ESnd((), Box::new($x), Box::new(b!()))
    }};
}

#[macro_export]
macro_rules! not {
    ( $x:literal ) => {{
        ANF::Neg(Box::new(b!(@anf $x)))
    }};
}

#[macro_export]
macro_rules! fst {
    ( $x:literal ) => {{
        fst!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        Expr::EFst((), Box::new($x), Box::new(b!()))
    }};
}
#[macro_export]
macro_rules! thd {
    ( $x:literal ) => {{
        thd!(b!(@anf $x))
    }};
    ( $x:expr ) => {{
        Expr::EPrj((), 2, Box::new($x), Box::new(b!()))
    }};
}
#[macro_export]
macro_rules! sample {
    ( $x:expr ) => {{
        Expr::ESample((), Box::new($x))
    }};
}
#[macro_export]
macro_rules! observe {
    ( $x:expr ) => {{
        if let Expr::EAnf(_, a) = $x {
            Expr::EObserve((), a)
        } else {
            panic!("passed in a non-anf expression!");
        }
    }};
}
#[macro_export]
macro_rules! flip {
    ( $num:literal / $denom:literal) => {{
        Expr::EFlip((), $num as f64 / $denom as f64)
    }};
    ( $p:literal ) => {{
        Expr::EFlip($p)
    }};
}

#[macro_export]
macro_rules! lets {
    ( $var:literal : $vty:ty := $bound:expr ; in $body:expr ; $ty:ty ) => {{
        Expr::ELetIn((),
            $var.to_string(),
            Box::new(typ!(TypeId::of::<$vty>())),
            Box::new($bound.clone()),
            Box::new($body.clone()),
            Box::new(typ!(TypeId::of::<$vty>())),
        )
    }};
    ( $( $var:literal : $fromty:ty := $bound:expr => $toty:ty);+ ;...? $body:expr ; $finty:ty ) => {
            {
                let mut fin = Box::new($body.clone());
                let mut bindees = vec![];
                $(
                    debug!("(let {} : {:?} = {:?})", $var.clone(), TypeId::of::<$vty>(), $bound.clone());
                    bindees.push(($var, TypeId::of::<$vty>(), $bound));
                )+
                debug!("...? {:?} ; {:?}", $body, TypeId::of::<$ty>());
                for (v, tyid, e) in bindees.iter().rev() {
                    fin = Box::new(Expr::ELetIn((), v.to_string(), Box::new(typ!(tyid)), Box::new(e.clone()), fin, Box::new(typ!(tyid))));
                }
                *fin
            }
        };
    // ( $( $var:literal : $fromty:ty := $bound:expr);+ ;...? $body:expr ; $finty:ty ) => {
    //         {
    //             let mut bindees = vec![];
    //             let mut types = vec![];
    //             let fintype = Box::new(typ!($finty));
    //             $(
    //                 debug!("(let {} : {:?} = {:?})", $var.clone(), TypeId::of::<$fromty>(), $bound.clone());
    //                 bindees.push(($var, TypeId::of::<$fromty>(), $bound));
    //                 types.push(($var, typ!(TypeId::of::<$fromty>())));
    //             )+
    //             debug!("...? {:?} ; {:?}", $body, TypeId::of::<$finty>());
    //             let mut fin = Box::new($body.clone());

    //             for (v, cotyid, e) in bindees.iter().rev() {
    //                 fin = Box::new(Expr::ELetIn(v.to_string(), Box::new(typ!(tyid)), Box::new(e.clone()), fin, fintype.clone()));
    //             }
    //             *fin
    //         }
    //     };

    ( $( $var:literal ; $fromty:expr ;= $bound:expr);+ ;...? $body:expr ; $finty:expr ) => {
            {
                let mut bindees = vec![];
                let fintype = Box::new($finty);
                $(
                    debug!("(let {} : {:?} = {:?})", $var.clone(), $fromty, $bound.clone());
                    bindees.push(($var, $fromty, $bound));
                )+
                debug!("...? {:?} ; {:?}", $body, $finty);
                let mut fin = Box::new($body.clone());

                for (v, tyid, e) in bindees.iter().rev() {
                    fin = Box::new(Expr::ELetIn((), v.to_string(), Box::new(tyid.clone()), Box::new(e.clone()), fin, fintype.clone()));
                }
                *fin
            }
        };

}
#[macro_export]
macro_rules! ite {
    ( if ( $pred:expr ) then { $true:expr } else { $false:expr } ) => {
        if let Expr::EAnf((), a) = $pred {
            Expr::EIte((), a, Box::new($true), Box::new($false), Box::new(b!()))
        } else {
            panic!("passed in a non-anf expression as predicate!");
        }
    };
    ( ( $pred:expr ) ? ( $true:expr ) : ( $false:expr ) ) => {
        Expr::EIte(
            (),
            Box::new($pred),
            Box::new($true),
            Box::new($false),
            Box::new(b!()),
        )
        // if let Expr::EAnf(a) = $pred {
        //     Expr::EIte($pred, Box::new($true), Box::new($false), Box::new(b!()))
        // } else {
        //     panic!("passed in a non-anf expression as predicate!");
        // }
    };
}

#[macro_export]
macro_rules! program {
    ( $x:expr ) => {
        Program::Body($x)
    };
}
// #[macro_export]
// macro_rules! run {
//     ( $( $var:literal := $bound:expr );+ ;...? $body:expr ) => {
//         program!($( $var:literal := $bound:expr );+ ;...? $body:expr)
//     };
// }
