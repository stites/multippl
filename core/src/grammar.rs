#![allow(non_camel_case_types)]

/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
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

pub enum ANF<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
{
    AVar(<AVarExt as ξ<X>>::Ext, String),
    AVal(<AValExt as ξ<X>>::Ext, Val),

    // TODO: not sure this is where I should add booleans, but it makes
    // the observe statements stay closer to the semantics: ~observe anf~
    And(Box<ANF<X>>, Box<ANF<X>>),
    Or(Box<ANF<X>>, Box<ANF<X>>),
    Neg(Box<ANF<X>>),
}

impl<X> Debug for ANF<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Debug,
    <AValExt as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ANF::*;
        match self {
            AVar(ext, s) => f.write_fmt(format_args!("Var {}", s)),
            AVal(ext, v) => f.write_fmt(format_args!("Val {:?}", v)),
            And(l, r) => f.write_fmt(format_args!("And({:?} && {:?})", l, r)),
            Or(l, r) => f.write_fmt(format_args!("Or({:?} || {:?})", l, r)),

            Neg(n) => f.write_fmt(format_args!("!({:?})", n)),
        }
    }
}
impl<X> Clone for ANF<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Clone,
    <AValExt as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use ANF::*;
        match self {
            AVar(ext, x) => AVar(ext.clone(), x.clone()),
            AVal(ext, x) => AVal(ext.clone(), x.clone()),
            And(l, r) => And(l.clone(), r.clone()),
            Or(l, r) => Or(l.clone(), r.clone()),
            Neg(x) => Neg(x.clone()),
        }
    }
}
impl<X> PartialEq for ANF<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: PartialEq,
    <AValExt as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use ANF::*;
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
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
{
    EAnf(<EAnfExt as ξ<X>>::Ext, Box<ANF<X>>),

    EFst(<EFstExt as ξ<X>>::Ext, Box<ANF<X>>),
    ESnd(<ESndExt as ξ<X>>::Ext, Box<ANF<X>>),
    EPrj(<EPrjExt as ξ<X>>::Ext, usize, Box<ANF<X>>),
    EProd(<EProdExt as ξ<X>>::Ext, Vec<ANF<X>>),

    // TODO Ignore function calls for now
    // EApp(String, Box<ANF>),
    ELetIn(<ELetInExt as ξ<X>>::Ext, String, Box<Expr<X>>, Box<Expr<X>>),
    EIte(
        <EIteExt as ξ<X>>::Ext,
        Box<ANF<X>>,
        Box<Expr<X>>,
        Box<Expr<X>>,
    ),
    EFlip(<EFlipExt as ξ<X>>::Ext, f64),
    EObserve(<EObserveExt as ξ<X>>::Ext, Box<ANF<X>>),
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
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
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
    <AVarExt as ξ<X>>::Ext: Debug,
    <AValExt as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        match self {
            EAnf(ext, a) => f.write_fmt(format_args!("Anf({:?})", a)),
            EFst(ext, a) => f
                .debug_tuple("Fst")
                // .field("ext", &ext)
                .field(a)
                .finish(),
            ESnd(ext, a) => f
                .debug_tuple("Snd")
                // .field("ext", &ext)
                .field(a)
                .finish(),
            EPrj(ext, i, a) => f
                .debug_tuple("Prj")
                // .field("ext", &ext)
                .field(i)
                .field(a)
                .finish(),
            EProd(ext, anfs) => f
                .debug_tuple("Prod")
                // .field("ext", &ext)
                .field(anfs)
                .finish(),
            ELetIn(ext, s, bindee, body) => f
                .debug_struct("LetIn")
                // .field("ext", &ext)
                .field("var", s)
                .field("bindee", bindee)
                .field("body", body)
                .finish(),
            EIte(ext, p, tru, fls) => f
                .debug_struct("Ite")
                // .field("ext", &ext)
                .field("predicate", p)
                .field("truthy", tru)
                .field("falsey", fls)
                .finish(),
            EFlip(ext, p) => f.write_fmt(format_args!("Flip {:?}", p)),
            EObserve(ext, a) => f
                .debug_tuple("Observe")
                // .field("ext", &ext)
                .field(a)
                .finish(),
            ESample(ext, e) => f
                .debug_tuple("Sample")
                // .field("ext", &ext)
                .field(e)
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
    AVarExt: ξ<X>,
    AValExt: ξ<X>,

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
    <AVarExt as ξ<X>>::Ext: Clone,
    <AValExt as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use Expr::*;
        match self {
            EAnf(ext, a) => EAnf(ext.clone(), a.clone()),
            EFst(ext, a) => EFst(ext.clone(), a.clone()),
            ESnd(ext, a) => ESnd(ext.clone(), a.clone()),
            EPrj(ext, i, a) => EPrj(ext.clone(), i.clone(), a.clone()),
            EProd(ext, anfs) => EProd(ext.clone(), anfs.clone()),
            ELetIn(ext, s, bindee, body) => {
                ELetIn(ext.clone(), s.clone(), bindee.clone(), body.clone())
            }
            EIte(ext, p, tru, fls) => EIte(ext.clone(), p.clone(), tru.clone(), fls.clone()),
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
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: PartialEq,
    <AValExt as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use Expr::*;
        match (self, o) {
            (EAnf(ext0, a0), EAnf(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (EFst(ext0, a0), EFst(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (ESnd(ext0, a0), ESnd(ext1, a1)) => ext0 == ext1 && a0 == a1,
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

impl<X: 'static> Arbitrary for ANF<X>
where
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Arbitrary,
    <AValExt as ξ<X>>::Ext: Arbitrary,
{
    fn arbitrary(g: &mut Gen) -> Self {
        let x = g.choose(&[0, 1, 2_u8]).copied();
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(0) => ANF::<X>::AVar(Arbitrary::arbitrary(g), String::arbitrary(g)),
            Some(1) => ANF::AVal(Arbitrary::arbitrary(g), Val::arbitrary(g)),
            Some(2) => {
                let x = g.choose(&[0, 1, 2_u8]);
                match x {
                    None => panic!("impossible: choose vec has len > 0"),
                    Some(0) => ANF::And(Box::<ANF<X>>::arbitrary(g), Box::<ANF<X>>::arbitrary(g)),
                    Some(1) => ANF::Or(Box::<ANF<X>>::arbitrary(g), Box::<ANF<X>>::arbitrary(g)),
                    Some(2) => ANF::Neg(Box::<ANF<X>>::arbitrary(g)),
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
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Arbitrary,
    <AValExt as ξ<X>>::Ext: Arbitrary,
{
    fn arbitrary(g: &mut Gen) -> Expr<X> {
        let x = g.choose(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]).copied();
        match x {
            None => panic!("impossible: choose vec has len > 0"),
            Some(0) => Expr::EAnf(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            Some(1) => Expr::EFst(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            Some(2) => Expr::ESnd(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)),
            Some(3) => Expr::EProd(
                Arbitrary::arbitrary(g),
                (0..2).map(|_i| ANF::arbitrary(g)).collect_vec(),
            ),
            Some(4) => {
                let var = String::arbitrary(g);
                let bind = Arbitrary::arbitrary(g);
                let body = Arbitrary::arbitrary(g);
                Expr::ELetIn(Arbitrary::arbitrary(g), var, bind, body)
            }
            Some(5) => {
                let p = Arbitrary::arbitrary(g);
                let t = Arbitrary::arbitrary(g);
                let f = Arbitrary::arbitrary(g);
                Expr::EIte(Arbitrary::arbitrary(g), p, t, f)
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
impl<X> Arbitrary for Program<X>
where
    X: Clone + 'static,
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
    <EAnfExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <EFstExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <ESndExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <EPrjExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <EProdExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <ELetInExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <EIteExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <EFlipExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <EObserveExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <ESampleExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
    <AValExt as ξ<X>>::Ext: Arbitrary + Debug + Clone,
{
    fn arbitrary(g: &mut Gen) -> Program<X> {
        Program::Body(Expr::arbitrary(g))
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
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Debug + Clone,
    <AValExt as ξ<X>>::Ext: Debug + Clone,
{
    pub fn is_sample(&self) -> bool {
        use Expr::*;
        match self {
            ESample(_, _) => true,
            _ => false,
        }
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
}

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
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Debug + Clone,
    <AValExt as ξ<X>>::Ext: Debug + Clone,
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
    AVarExt: ξ<X>,
    AValExt: ξ<X>,
    <AVarExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <AValExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
{
    pub fn strip_samples(&self) -> Program<X> {
        use Program::*;
        match self {
            Body(e) => {
                // FIXME: this shouldn't be neccesary and I think I already fixed the bug that causes this.
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

pub type AnfUD = ANF<UD>;
pub type ExprUD = Expr<UD>;
pub type ProgramUD = Program<UD>;
