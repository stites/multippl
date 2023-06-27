#![allow(non_camel_case_types)]

/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
use crate::*;
use ::core::fmt;
use ::core::fmt::Debug;
use itertools::Itertools;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::string::String;

#[derive(Debug, Clone, PartialEq)]
pub enum ETy {
    EBool,
    EProd(Vec<ETy>),
}
impl ETy {
    pub fn right(&self) -> Option<ETy> {
        match self {
            ETy::EBool => None,
            ETy::EProd(tys) => match &tys[..] {
                [_, r] => Some(r.clone()),
                _ => None,
            },
        }
    }
    pub fn left(&self) -> Option<ETy> {
        match self {
            ETy::EBool => None,
            ETy::EProd(tys) => match &tys[..] {
                [l, _] => Some(l.clone()),
                _ => None,
            },
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum STy {
    SBool,
    SFloat,
    SInt,
    SVec(Vec<STy>),
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
pub struct AVarExt<Val> {
    vartype: PhantomData<Val>,
}
pub struct AValExt<Val> {
    valtype: PhantomData<Val>,
}

pub struct SAnfExt;
pub struct SLetInExt;
pub struct SSeqExt;
pub struct SIteExt;
pub struct SReturnExt;
pub struct SBernExt;
pub struct SDiscreteExt;
pub struct SUniformExt;
pub struct SNormalExt;
pub struct SBetaExt;
pub struct SDirichletExt;
pub struct SObserveExt;
pub struct SExactExt;

#[derive(Debug, Clone, PartialEq)]
pub enum EVal {
    EBool(bool),
    EProd(Vec<EVal>),
}
impl EVal {
    pub fn as_bool(&self) -> Option<bool> {
        use EVal::*;
        match self {
            EBool(b) => Some(*b),
            EProd(_) => None,
        }
    }
    pub fn is_true(&self) -> bool {
        use EVal::*;
        match self {
            EBool(true) => true,
            _ => false,
        }
    }
}

pub trait IsTyped<T: PartialEq> {
    fn is_prod(&self) -> bool;
    fn as_type(&self) -> T;
    fn is_type(&self, ty: &T) -> bool {
        self.as_type() == *ty
    }
}
impl IsTyped<ETy> for EVal {
    fn is_prod(&self) -> bool {
        use EVal::*;
        match self {
            EBool(_) => false,
            EProd(_) => true,
        }
    }
    fn as_type(&self) -> ETy {
        use EVal::*;
        match self {
            EBool(_) => ETy::EBool,
            EProd(vs) => ETy::EProd(vs.iter().map(|x| x.as_type()).collect_vec()),
        }
    }
}

pub enum Anf<X, Val>
where
    AVarExt<Val>: ξ<X>,
    AValExt<Val>: ξ<X>,
{
    AVar(<AVarExt<Val> as ξ<X>>::Ext, String),
    AVal(<AValExt<Val> as ξ<X>>::Ext, Val),

    // Boolean ops
    And(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    Or(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    Neg(Box<Anf<X, Val>>),

    // numerics:
    // NOTE: exact compares differently-sized one-hot encodings, searching for the first non-empty element
    Plus(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    Minus(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    Mult(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    Div(Box<Anf<X, Val>>, Box<Anf<X, Val>>),

    // Ord
    GT(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    LT(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    GTE(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    LTE(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    EQ(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
}
impl<X, Val> Debug for Anf<X, Val>
where
    AVarExt<Val>: ξ<X>,
    AValExt<Val>: ξ<X>,
    Val: Debug,
    <AVarExt<Val> as ξ<X>>::Ext: Debug,
    <AValExt<Val> as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Anf::*;
        match self {
            AVar(ext, s) => f.write_fmt(format_args!("Var({:?}) {}", ext, s)),
            AVal(ext, v) => f.write_fmt(format_args!("EVal({:?}) {:?}", ext, v)),
            And(l, r) => f.write_fmt(format_args!("And({:?} && {:?})", l, r)),
            Or(l, r) => f.write_fmt(format_args!("Or({:?} || {:?})", l, r)),
            Neg(n) => f.write_fmt(format_args!("!({:?})", n)),
            Plus(l, r) => f.write_fmt(format_args!("{:?} + {:?}", l, r)),
            Minus(l, r) => f.write_fmt(format_args!("{:?} - {:?}", l, r)),
            Mult(l, r) => f.write_fmt(format_args!("{:?} * {:?}", l, r)),
            Div(l, r) => f.write_fmt(format_args!("{:?} / {:?}", l, r)),
            GT(l, r) => f.write_fmt(format_args!("{:?} > {:?}", l, r)),
            LT(l, r) => f.write_fmt(format_args!("{:?} < {:?}", l, r)),
            GTE(l, r) => f.write_fmt(format_args!("{:?} >= {:?}", l, r)),
            LTE(l, r) => f.write_fmt(format_args!("{:?} <= {:?}", l, r)),
            EQ(l, r) => f.write_fmt(format_args!("{:?} == {:?}", l, r)),
        }
    }
}
impl<X, Val> Clone for Anf<X, Val>
where
    AVarExt<Val>: ξ<X>,
    AValExt<Val>: ξ<X>,
    Val: Clone,
    <AVarExt<Val> as ξ<X>>::Ext: Clone,
    <AValExt<Val> as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use Anf::*;
        match self {
            AVar(ext, x) => AVar(ext.clone(), x.clone()),
            AVal(ext, x) => AVal(ext.clone(), x.clone()),
            And(l, r) => And(l.clone(), r.clone()),
            Or(l, r) => Or(l.clone(), r.clone()),
            Neg(x) => Neg(x.clone()),
            Plus(l, r) => Plus(l.clone(), r.clone()),
            Minus(l, r) => Minus(l.clone(), r.clone()),
            Mult(l, r) => Mult(l.clone(), r.clone()),
            Div(l, r) => Div(l.clone(), r.clone()),
            GT(l, r) => GT(l.clone(), r.clone()),
            LT(l, r) => LT(l.clone(), r.clone()),
            GTE(l, r) => GTE(l.clone(), r.clone()),
            LTE(l, r) => LTE(l.clone(), r.clone()),
            EQ(l, r) => EQ(l.clone(), r.clone()),
        }
    }
}
impl<X, Val> PartialEq for Anf<X, Val>
where
    AVarExt<Val>: ξ<X>,
    AValExt<Val>: ξ<X>,
    Val: PartialEq,
    <AVarExt<Val> as ξ<X>>::Ext: PartialEq,
    <AValExt<Val> as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use Anf::*;
        match (self, o) {
            (AVar(ext0, a0), AVar(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (AVal(ext0, a0), AVal(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (And(l0, r0), And(l1, r1)) => l0 == l1 && r0 == r1,
            (Or(l0, r0), Or(l1, r1)) => l0 == l1 && r0 == r1,
            (Neg(a0), Neg(a1)) => a0 == a1,
            (Plus(l0, r0), Plus(l1, r1)) => l0 == l1 && r0 == r1,
            (Minus(l0, r0), Minus(l1, r1)) => l0 == l1 && r0 == r1,
            (Mult(l0, r0), Mult(l1, r1)) => l0 == l1 && r0 == r1,
            (Div(l0, r0), Div(l1, r1)) => l0 == l1 && r0 == r1,
            (GT(l0, r0), GT(l1, r1)) => l0 == l1 && r0 == r1,
            (LT(l0, r0), LT(l1, r1)) => l0 == l1 && r0 == r1,
            (GTE(l0, r0), GTE(l1, r1)) => l0 == l1 && r0 == r1,
            (LTE(l0, r0), LTE(l1, r1)) => l0 == l1 && r0 == r1,
            (EQ(l0, r0), EQ(l1, r1)) => l0 == l1 && r0 == r1,
            (_, _) => false,
        }
    }
}

#[allow(clippy::enum_variant_names)]
pub enum EExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    AVarExt<EVal>: ξ<X>,
    AValExt<EVal>: ξ<X>,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    SExactExt: ξ<X>,
    SObserveExt: ξ<X>,
    AValExt<SVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
{
    EAnf(<EAnfExt as ξ<X>>::Ext, Box<Anf<X, EVal>>),

    EPrj(<EPrjExt as ξ<X>>::Ext, usize, Box<Anf<X, EVal>>),
    EProd(<EProdExt as ξ<X>>::Ext, Vec<Anf<X, EVal>>),

    // TODO Ignore function calls for now
    // EApp(String, Box<Anf>),
    ELetIn(
        <ELetInExt as ξ<X>>::Ext,
        String,
        Box<EExpr<X>>,
        Box<EExpr<X>>,
    ),
    EIte(
        <EIteExt as ξ<X>>::Ext,
        Box<Anf<X, EVal>>,
        Box<EExpr<X>>,
        Box<EExpr<X>>,
    ),
    EFlip(<EFlipExt as ξ<X>>::Ext, f64),
    EObserve(<EObserveExt as ξ<X>>::Ext, Box<Anf<X, EVal>>),
    ESample(<ESampleExt as ξ<X>>::Ext, Box<SExpr<X>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SVal {
    SBool(bool),
    SFloat(f64),
    // SFloatVec(Vec<f64>),
    SInt(u64),
}
impl IsTyped<STy> for SVal {
    fn is_prod(&self) -> bool {
        false
    }
    fn as_type(&self) -> STy {
        use SVal::*;
        match self {
            SBool(_) => STy::SBool,
            SInt(_) => STy::SInt,
            SFloat(_) => STy::SFloat,
            // SFloatVec(_) => STy::SFloatVec,
        }
    }
}
impl SVal {
    pub fn as_query(&self) -> f64 {
        use SVal::*;
        match self {
            SBool(x) => {
                if *x {
                    1.0
                } else {
                    0.0
                }
            }
            SInt(x) => *x as f64,
            SFloat(x) => *x,
            // SFloatVec(vs) => vs.clone(),
        }
    }
    pub fn from_bools(bs: &Vec<bool>) -> Vec<Self> {
        bs.iter().cloned().map(SVal::SBool).collect()
    }
    pub fn as_bool(&self) -> bool {
        use SVal::*;
        match self {
            SBool(b) => *b,
            _ => panic!("type-checking says this is impossible!"),
        }
    }
    pub fn as_float(&self) -> f64 {
        use SVal::*;
        match self {
            SFloat(b) => *b,
            _ => panic!("type-checking says this is impossible!"),
        }
    }
    pub fn as_int(&self) -> u64 {
        use SVal::*;
        match self {
            SInt(b) => *b,
            _ => panic!("type-checking says this is impossible!"),
        }
    }
    pub fn vec_as<X>(v: &Vec<SVal>, f: &dyn Fn(&SVal) -> X) -> X {
        assert!(Self::vec_is_single_val(v));
        // assert!(Self::vec_is(v, f));
        f(&v[0])
    }
    // make this safe if you have more time.
    // pub fn vec_is<X>(v: &Vec<SVal>, f:Fn(SVal)->X) -> bool {
    //     f(v[0]); // this is an unsafe method, so if this doesn't panic we're good
    //     true
    // }
    fn vec_is_single_val(v: &Vec<SVal>) -> bool {
        v.len() == 1 && matches!(v[0].as_type(), STy::SVec(_))
    }
}

#[allow(clippy::enum_variant_names)]
pub enum SExpr<X>
where
    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    SObserveExt: ξ<X>,
    SExactExt: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,

    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    AVarExt<EVal>: ξ<X>,
    AValExt<EVal>: ξ<X>,
{
    SAnf(<SAnfExt as ξ<X>>::Ext, Box<Anf<X, SVal>>),
    SLetIn(
        <SLetInExt as ξ<X>>::Ext,
        String,
        Box<SExpr<X>>,
        Box<SExpr<X>>,
    ),
    SSeq(<SSeqExt as ξ<X>>::Ext, Box<SExpr<X>>, Box<SExpr<X>>),
    SIte(
        <SIteExt as ξ<X>>::Ext,
        Box<Anf<X, SVal>>,
        Box<SExpr<X>>,
        Box<SExpr<X>>,
    ),

    // distributions
    SBern(<SBernExt as ξ<X>>::Ext, Box<Anf<X, SVal>>),
    SDiscrete(<SDiscreteExt as ξ<X>>::Ext, Vec<Anf<X, SVal>>),
    SUniform(
        <SUniformExt as ξ<X>>::Ext,
        Box<Anf<X, SVal>>,
        Box<Anf<X, SVal>>,
    ),
    SNormal(
        <SNormalExt as ξ<X>>::Ext,
        Box<Anf<X, SVal>>,
        Box<Anf<X, SVal>>,
    ),
    SBeta(
        <SBetaExt as ξ<X>>::Ext,
        Box<Anf<X, SVal>>,
        Box<Anf<X, SVal>>,
    ),
    SDirichlet(<SDirichletExt as ξ<X>>::Ext, Vec<Anf<X, SVal>>),

    // Morally, this is an "observe extension" of every distribution. For
    // example:
    // - ObserveBern(<value>, <float>)
    // - ObserveDiscrete(<value>, Vec<float>)
    // - ObserveUniform(<value>, <float>, <float>)
    // etc.
    //
    // We do this because it lets us keep values simple, and bars us from
    // reasoning about sampler subprograms. We do get a constrained form of
    // these through SExact(ESample) statements.
    SObserve(<SObserveExt as ξ<X>>::Ext, Box<Anf<X, SVal>>, Box<SExpr<X>>),

    // Multi-language boundary
    SExact(<SExactExt as ξ<X>>::Ext, Box<EExpr<X>>),
}

impl<X> Debug for EExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug,
    <EPrjExt as ξ<X>>::Ext: Debug,
    <EProdExt as ξ<X>>::Ext: Debug,
    <ELetInExt as ξ<X>>::Ext: Debug,
    <EIteExt as ξ<X>>::Ext: Debug,
    <EFlipExt as ξ<X>>::Ext: Debug,
    <EObserveExt as ξ<X>>::Ext: Debug,
    <ESampleExt as ξ<X>>::Ext: Debug,
    <AVarExt<EVal> as ξ<X>>::Ext: Debug,
    <AVarExt<SVal> as ξ<X>>::Ext: Debug,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Debug,
    <AValExt<SVal> as ξ<X>>::Ext: Debug,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Debug,
    <SDiscreteExt as ξ<X>>::Ext: Debug,
    <SUniformExt as ξ<X>>::Ext: Debug,
    <SNormalExt as ξ<X>>::Ext: Debug,
    <SBetaExt as ξ<X>>::Ext: Debug,
    <SDirichletExt as ξ<X>>::Ext: Debug,

    SObserveExt: ξ<X>,
    SExactExt: ξ<X>,
    <SObserveExt as ξ<X>>::Ext: Debug,
    <SAnfExt as ξ<X>>::Ext: Debug,
    <SLetInExt as ξ<X>>::Ext: Debug,
    <SSeqExt as ξ<X>>::Ext: Debug,
    <SIteExt as ξ<X>>::Ext: Debug,
    <SExactExt as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EExpr::*;
        match self {
            EAnf(ext, a) => f.write_fmt(format_args!("Anf({:?},{:?})", ext, a)),
            EPrj(ext, i, a) => f.debug_tuple("Prj").field(&ext).field(i).field(a).finish(),
            EProd(ext, anfs) => f.debug_tuple("EProd").field(&ext).field(anfs).finish(),
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

impl<X> Debug for SExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug,
    <EPrjExt as ξ<X>>::Ext: Debug,
    <EProdExt as ξ<X>>::Ext: Debug,
    <ELetInExt as ξ<X>>::Ext: Debug,
    <EIteExt as ξ<X>>::Ext: Debug,
    <EFlipExt as ξ<X>>::Ext: Debug,
    <EObserveExt as ξ<X>>::Ext: Debug,
    <SObserveExt as ξ<X>>::Ext: Debug,
    <ESampleExt as ξ<X>>::Ext: Debug,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Debug,
    <SLetInExt as ξ<X>>::Ext: Debug,
    <SSeqExt as ξ<X>>::Ext: Debug,
    <SIteExt as ξ<X>>::Ext: Debug,
    <SExactExt as ξ<X>>::Ext: Debug,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Debug,
    <SDiscreteExt as ξ<X>>::Ext: Debug,
    <SUniformExt as ξ<X>>::Ext: Debug,
    <SNormalExt as ξ<X>>::Ext: Debug,
    <SBetaExt as ξ<X>>::Ext: Debug,
    <SDirichletExt as ξ<X>>::Ext: Debug,

    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Debug,
    <AVarExt<SVal> as ξ<X>>::Ext: Debug,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Debug,
    <AValExt<SVal> as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SExpr::*;
        match self {
            SAnf(ext, a) => f.write_fmt(format_args!("Anf({:?},{:?})", ext, a)),
            SLetIn(ext, s, bindee, body) => f
                .debug_struct("SLetIn")
                .field("ext", &ext)
                .field("var", s)
                .field("bindee", bindee)
                .field("body", body)
                .finish(),
            SSeq(ext, e1, e2) => f
                .debug_struct("SSeq")
                .field("ext", &ext)
                .field("e1", e1)
                .field("e2", e2)
                .finish(),
            SIte(ext, p, tru, fls) => f
                .debug_struct("Ite")
                .field("ext", &ext)
                .field("predicate", p)
                .field("truthy", tru)
                .field("falsey", fls)
                .finish(),
            SBern(ext, p) => f.write_fmt(format_args!("Bern({:?}, {:?})", ext, p)),
            SDiscrete(ext, ps) => f.write_fmt(format_args!("Discrete({:?}, {:?})", ext, ps)),
            SUniform(ext, lo, hi) => {
                f.write_fmt(format_args!("Uniform({:?}, {:?}, {:?})", ext, lo, hi))
            }
            SNormal(ext, mean, var) => {
                f.write_fmt(format_args!("Normal({:?}, {:?}, {:?})", ext, mean, var))
            }
            SBeta(ext, a, b) => f.write_fmt(format_args!("Beta({:?}, {:?}, {:?})", ext, a, b)),
            SDirichlet(ext, ps) => f.write_fmt(format_args!("Dirichlet({:?}, {:?})", ext, ps)),
            SObserve(ext, a, e) => {
                f.write_fmt(format_args!("Observe({:?}) {:?} from {:?})", ext, a, e))
            }

            SExact(ext, e) => f.debug_tuple("Exact").field(&ext).field(e).finish(),
        }
    }
}

impl<X> Clone for EExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,

    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Clone,
    <AVarExt<SVal> as ξ<X>>::Ext: Clone,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Clone,
    <AValExt<SVal> as ξ<X>>::Ext: Clone,

    <EAnfExt as ξ<X>>::Ext: Clone,
    <EPrjExt as ξ<X>>::Ext: Clone,
    <EProdExt as ξ<X>>::Ext: Clone,
    <ELetInExt as ξ<X>>::Ext: Clone,
    <EIteExt as ξ<X>>::Ext: Clone,
    <EFlipExt as ξ<X>>::Ext: Clone,
    <EObserveExt as ξ<X>>::Ext: Clone,
    <SObserveExt as ξ<X>>::Ext: Clone,
    <ESampleExt as ξ<X>>::Ext: Clone,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Clone,
    <SDiscreteExt as ξ<X>>::Ext: Clone,
    <SUniformExt as ξ<X>>::Ext: Clone,
    <SNormalExt as ξ<X>>::Ext: Clone,
    <SBetaExt as ξ<X>>::Ext: Clone,
    <SDirichletExt as ξ<X>>::Ext: Clone,

    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Clone,
    <SLetInExt as ξ<X>>::Ext: Clone,
    <SSeqExt as ξ<X>>::Ext: Clone,
    <SIteExt as ξ<X>>::Ext: Clone,
    <SExactExt as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use EExpr::*;
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

impl<X> Clone for SExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Clone,
    <AVarExt<SVal> as ξ<X>>::Ext: Clone,

    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Clone,
    <AValExt<SVal> as ξ<X>>::Ext: Clone,

    <EAnfExt as ξ<X>>::Ext: Clone,
    <EPrjExt as ξ<X>>::Ext: Clone,
    <EProdExt as ξ<X>>::Ext: Clone,
    <ELetInExt as ξ<X>>::Ext: Clone,
    <EIteExt as ξ<X>>::Ext: Clone,
    <EFlipExt as ξ<X>>::Ext: Clone,
    <EObserveExt as ξ<X>>::Ext: Clone,
    <SObserveExt as ξ<X>>::Ext: Clone,
    <ESampleExt as ξ<X>>::Ext: Clone,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Clone,
    <SLetInExt as ξ<X>>::Ext: Clone,
    <SSeqExt as ξ<X>>::Ext: Clone,
    <SIteExt as ξ<X>>::Ext: Clone,
    <SExactExt as ξ<X>>::Ext: Clone,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Clone,
    <SDiscreteExt as ξ<X>>::Ext: Clone,
    <SUniformExt as ξ<X>>::Ext: Clone,
    <SNormalExt as ξ<X>>::Ext: Clone,
    <SBetaExt as ξ<X>>::Ext: Clone,
    <SDirichletExt as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use SExpr::*;
        match self {
            SAnf(ext, a) => SAnf(ext.clone(), a.clone()),
            SLetIn(ext, s, bindee, body) => {
                SLetIn(ext.clone(), s.clone(), bindee.clone(), body.clone())
            }
            SSeq(ext, e1, e2) => SSeq(ext.clone(), e1.clone(), e2.clone()),
            SIte(ext, p, tru, fls) => SIte(ext.clone(), p.clone(), tru.clone(), fls.clone()),
            SBern(ext, p) => SBern(ext.clone(), p.clone()),
            SDiscrete(ext, ps) => SDiscrete(ext.clone(), ps.clone()),
            SUniform(ext, lo, hi) => SUniform(ext.clone(), lo.clone(), hi.clone()),
            SNormal(ext, mean, var) => SNormal(ext.clone(), mean.clone(), var.clone()),
            SBeta(ext, a, b) => SBeta(ext.clone(), a.clone(), b.clone()),
            SDirichlet(ext, ps) => SDirichlet(ext.clone(), ps.clone()),
            SObserve(ext, a, b) => SObserve(ext.clone(), a.clone(), b.clone()),
            SExact(ext, e) => SExact(ext.clone(), e.clone()),
        }
    }
}
impl<X> PartialEq for EExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,

    <EAnfExt as ξ<X>>::Ext: PartialEq,
    <EPrjExt as ξ<X>>::Ext: PartialEq,
    <EProdExt as ξ<X>>::Ext: PartialEq,
    <ELetInExt as ξ<X>>::Ext: PartialEq,
    <EIteExt as ξ<X>>::Ext: PartialEq,
    <EFlipExt as ξ<X>>::Ext: PartialEq,
    <EObserveExt as ξ<X>>::Ext: PartialEq,
    <SObserveExt as ξ<X>>::Ext: PartialEq,
    <ESampleExt as ξ<X>>::Ext: PartialEq,
    AVarExt<SVal>: ξ<X>,
    AVarExt<EVal>: ξ<X>,
    <AVarExt<SVal> as ξ<X>>::Ext: PartialEq,
    <AVarExt<EVal> as ξ<X>>::Ext: PartialEq,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: PartialEq,
    <AValExt<SVal> as ξ<X>>::Ext: PartialEq,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: PartialEq,
    <SLetInExt as ξ<X>>::Ext: PartialEq,
    <SSeqExt as ξ<X>>::Ext: PartialEq,
    <SIteExt as ξ<X>>::Ext: PartialEq,
    <SExactExt as ξ<X>>::Ext: PartialEq,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: PartialEq,
    <SDiscreteExt as ξ<X>>::Ext: PartialEq,
    <SUniformExt as ξ<X>>::Ext: PartialEq,
    <SNormalExt as ξ<X>>::Ext: PartialEq,
    <SBetaExt as ξ<X>>::Ext: PartialEq,
    <SDirichletExt as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use EExpr::*;
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

impl<X> PartialEq for SExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,

    <EAnfExt as ξ<X>>::Ext: PartialEq,
    <EPrjExt as ξ<X>>::Ext: PartialEq,
    <EProdExt as ξ<X>>::Ext: PartialEq,
    <ELetInExt as ξ<X>>::Ext: PartialEq,
    <EIteExt as ξ<X>>::Ext: PartialEq,
    <EFlipExt as ξ<X>>::Ext: PartialEq,
    <EObserveExt as ξ<X>>::Ext: PartialEq,
    <SObserveExt as ξ<X>>::Ext: PartialEq,
    <ESampleExt as ξ<X>>::Ext: PartialEq,
    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: PartialEq,
    <AVarExt<SVal> as ξ<X>>::Ext: PartialEq,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: PartialEq,
    <AValExt<SVal> as ξ<X>>::Ext: PartialEq,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: PartialEq,
    <SLetInExt as ξ<X>>::Ext: PartialEq,
    <SSeqExt as ξ<X>>::Ext: PartialEq,
    <SIteExt as ξ<X>>::Ext: PartialEq,
    <SExactExt as ξ<X>>::Ext: PartialEq,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: PartialEq,
    <SDiscreteExt as ξ<X>>::Ext: PartialEq,
    <SUniformExt as ξ<X>>::Ext: PartialEq,
    <SNormalExt as ξ<X>>::Ext: PartialEq,
    <SBetaExt as ξ<X>>::Ext: PartialEq,
    <SDirichletExt as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use SExpr::*;
        match (self, o) {
            (SAnf(ext0, a0), SAnf(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (SSeq(ext0, e01, e02), SSeq(ext1, e11, e12)) => {
                ext0 == ext1 && e01 == e11 && e02 == e12
            }
            (SLetIn(ext0, s0, bindee0, body0), SLetIn(ext1, s1, bindee1, body1)) => {
                ext0 == ext1 && s0 == s1 && bindee0 == bindee1 && body0 == body1
            }
            (SIte(ext0, p0, tru0, fls0), SIte(ext1, p1, tru1, fls1)) => {
                ext0 == ext1 && p0 == p1 && tru0 == tru1 && fls0 == fls1
            }
            (SBern(ext0, a0), SBern(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (SDiscrete(ext0, a0), SDiscrete(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (SDirichlet(ext0, a0), SDirichlet(ext1, a1)) => ext0 == ext1 && a0 == a1,
            (SUniform(ext0, a0, b0), SUniform(ext1, a1, b1)) => {
                ext0 == ext1 && a0 == a1 && b0 == b1
            }
            (SNormal(ext0, a0, b0), SNormal(ext1, a1, b1)) => ext0 == ext1 && a0 == a1 && b0 == b1,
            (SBeta(ext0, a0, b0), SBeta(ext1, a1, b1)) => ext0 == ext1 && a0 == a1 && b0 == b1,
            (SObserve(ext0, a0, b0), SObserve(ext1, a1, b1)) => {
                ext0 == ext1 && a0 == a1 && b0 == b1
            }
            (SExact(ext0, e0), SExact(ext1, e1)) => ext0 == ext1 && e0 == e1,

            (_, _) => false,
        }
    }
}
#[derive(Default, Debug, Clone)]
pub struct Γ(pub Vec<(String, ETy)>);
impl Γ {
    pub fn get(&self, x: String) -> Option<ETy> {
        self.0
            .iter()
            .find(|(e, _)| *e == x)
            .map(|(_, t)| t)
            .cloned()
    }
    pub fn push(&mut self, x: String, ty: &ETy) {
        self.0.push((x, ty.clone()));
    }
    pub fn append(&self, x: String, ty: &ETy) -> Γ {
        let mut ctx = self.0.clone();
        ctx.push((x, ty.clone()));
        Γ(ctx)
    }
    pub fn typechecks(&self, x: String, ty: &ETy) -> bool {
        self.get(x).map(|t| t == *ty).is_some()
    }
}
// TODO
// structure Func where
//   name : String
//   arg : String × Ty
//   ret : Ty
//   body : EExpr
// deriving Repr

impl<X> EExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone,
    <EProdExt as ξ<X>>::Ext: Debug + Clone,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone,
    <EIteExt as ξ<X>>::Ext: Debug + Clone,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone,
    <SObserveExt as ξ<X>>::Ext: Debug + Clone,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone,
    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Debug + Clone,
    <AVarExt<SVal> as ξ<X>>::Ext: Debug + Clone,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Debug + Clone,
    <AValExt<SVal> as ξ<X>>::Ext: Debug + Clone,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Debug + Clone,
    <SLetInExt as ξ<X>>::Ext: Debug + Clone,
    <SSeqExt as ξ<X>>::Ext: Debug + Clone,
    <SIteExt as ξ<X>>::Ext: Debug + Clone,
    <SExactExt as ξ<X>>::Ext: Debug + Clone,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Debug + Clone,
    <SDiscreteExt as ξ<X>>::Ext: Debug + Clone,
    <SUniformExt as ξ<X>>::Ext: Debug + Clone,
    <SNormalExt as ξ<X>>::Ext: Debug + Clone,
    <SBetaExt as ξ<X>>::Ext: Debug + Clone,
    <SDirichletExt as ξ<X>>::Ext: Debug + Clone,
{
    pub fn is_sample(&self) -> bool {
        matches!(self, EExpr::ESample(_, _))
    }

    pub fn is_let(&self) -> bool {
        matches!(self, EExpr::ELetIn(_, _, _, _))
    }

    pub fn query(&self) -> EExpr<X> {
        use EExpr::*;
        match self {
            ELetIn(ex, s, x, y) => y.query(),
            _ => self.clone(),
        }
    }
    pub fn insert_observe(&self, e: EExpr<X>) -> EExpr<X> {
        use EExpr::*;
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

impl<X> SExpr<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone,
    <EProdExt as ξ<X>>::Ext: Debug + Clone,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone,
    <EIteExt as ξ<X>>::Ext: Debug + Clone,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone,
    <SObserveExt as ξ<X>>::Ext: Debug + Clone,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone,
    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Debug + Clone,
    <AVarExt<SVal> as ξ<X>>::Ext: Debug + Clone,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Debug + Clone,
    <AValExt<SVal> as ξ<X>>::Ext: Debug + Clone,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Debug + Clone,
    <SLetInExt as ξ<X>>::Ext: Debug + Clone,
    <SSeqExt as ξ<X>>::Ext: Debug + Clone,
    <SIteExt as ξ<X>>::Ext: Debug + Clone,
    <SExactExt as ξ<X>>::Ext: Debug + Clone,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Debug + Clone,
    <SDiscreteExt as ξ<X>>::Ext: Debug + Clone,
    <SUniformExt as ξ<X>>::Ext: Debug + Clone,
    <SNormalExt as ξ<X>>::Ext: Debug + Clone,
    <SBetaExt as ξ<X>>::Ext: Debug + Clone,
    <SDirichletExt as ξ<X>>::Ext: Debug + Clone,
{
    pub fn is_let(&self) -> bool {
        matches!(self, SExpr::SLetIn(_, _, _, _))
    }

    pub fn query(&self) -> SExpr<X> {
        use SExpr::*;
        match self {
            SLetIn(ex, s, x, y) => y.query(),
            _ => self.clone(),
        }
    }
}

pub enum Program<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,

    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,
{
    EBody(EExpr<X>),
    SBody(SExpr<X>),
    // TODO
    // | define (f: Func) (rest: Program) : Program
}

#[derive(Debug)]
pub enum Query<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,

    <EAnfExt as ξ<X>>::Ext: Debug,
    <EPrjExt as ξ<X>>::Ext: Debug,
    <EProdExt as ξ<X>>::Ext: Debug,
    <ELetInExt as ξ<X>>::Ext: Debug,
    <EIteExt as ξ<X>>::Ext: Debug,
    <EFlipExt as ξ<X>>::Ext: Debug,
    <EObserveExt as ξ<X>>::Ext: Debug,
    <SObserveExt as ξ<X>>::Ext: Debug,
    <ESampleExt as ξ<X>>::Ext: Debug,
    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Debug,
    <AVarExt<SVal> as ξ<X>>::Ext: Debug,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Debug,
    <AValExt<SVal> as ξ<X>>::Ext: Debug,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Debug,
    <SLetInExt as ξ<X>>::Ext: Debug,
    <SSeqExt as ξ<X>>::Ext: Debug,
    <SIteExt as ξ<X>>::Ext: Debug,
    <SExactExt as ξ<X>>::Ext: Debug,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Debug,
    <SDiscreteExt as ξ<X>>::Ext: Debug,
    <SUniformExt as ξ<X>>::Ext: Debug,
    <SNormalExt as ξ<X>>::Ext: Debug,
    <SBetaExt as ξ<X>>::Ext: Debug,
    <SDirichletExt as ξ<X>>::Ext: Debug,
{
    EQuery(EExpr<X>),
    SQuery(SExpr<X>),
}

impl<X> PartialEq for Program<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,

    <EAnfExt as ξ<X>>::Ext: PartialEq,
    <EPrjExt as ξ<X>>::Ext: PartialEq,
    <EProdExt as ξ<X>>::Ext: PartialEq,
    <ELetInExt as ξ<X>>::Ext: PartialEq,
    <EIteExt as ξ<X>>::Ext: PartialEq,
    <EFlipExt as ξ<X>>::Ext: PartialEq,
    <EObserveExt as ξ<X>>::Ext: PartialEq,
    <SObserveExt as ξ<X>>::Ext: PartialEq,
    <ESampleExt as ξ<X>>::Ext: PartialEq,
    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: PartialEq,
    <AVarExt<SVal> as ξ<X>>::Ext: PartialEq,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: PartialEq,
    <AValExt<SVal> as ξ<X>>::Ext: PartialEq,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: PartialEq,
    <SLetInExt as ξ<X>>::Ext: PartialEq,
    <SSeqExt as ξ<X>>::Ext: PartialEq,
    <SIteExt as ξ<X>>::Ext: PartialEq,
    <SExactExt as ξ<X>>::Ext: PartialEq,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: PartialEq,
    <SDiscreteExt as ξ<X>>::Ext: PartialEq,
    <SUniformExt as ξ<X>>::Ext: PartialEq,
    <SNormalExt as ξ<X>>::Ext: PartialEq,
    <SBetaExt as ξ<X>>::Ext: PartialEq,
    <SDirichletExt as ξ<X>>::Ext: PartialEq,
{
    fn eq(&self, o: &Self) -> bool {
        use Program::*;
        match (self, o) {
            (SBody(e0), SBody(e1)) => e0 == e1,
            (EBody(e0), EBody(e1)) => e0 == e1,
            (_, _) => false,
        }
    }
}
impl<X> Debug for Program<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug,
    <EPrjExt as ξ<X>>::Ext: Debug,
    <EProdExt as ξ<X>>::Ext: Debug,
    <ELetInExt as ξ<X>>::Ext: Debug,
    <EIteExt as ξ<X>>::Ext: Debug,
    <EFlipExt as ξ<X>>::Ext: Debug,
    <EObserveExt as ξ<X>>::Ext: Debug,
    <SObserveExt as ξ<X>>::Ext: Debug,
    <ESampleExt as ξ<X>>::Ext: Debug,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Debug,
    <SLetInExt as ξ<X>>::Ext: Debug,
    <SSeqExt as ξ<X>>::Ext: Debug,
    <SIteExt as ξ<X>>::Ext: Debug,
    <SExactExt as ξ<X>>::Ext: Debug,

    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Debug,
    <AVarExt<SVal> as ξ<X>>::Ext: Debug,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Debug,
    <AValExt<SVal> as ξ<X>>::Ext: Debug,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Debug,
    <SDiscreteExt as ξ<X>>::Ext: Debug,
    <SUniformExt as ξ<X>>::Ext: Debug,
    <SNormalExt as ξ<X>>::Ext: Debug,
    <SBetaExt as ξ<X>>::Ext: Debug,
    <SDirichletExt as ξ<X>>::Ext: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Program::*;
        match self {
            SBody(e) => f.write_fmt(format_args!("SProgram({:?})", e)),
            EBody(e) => f.write_fmt(format_args!("EProgram({:?})", e)),
        }
    }
}

impl<X> Clone for Program<X>
where
    EAnfExt: ξ<X>,
    EPrjExt: ξ<X>,
    EProdExt: ξ<X>,
    ELetInExt: ξ<X>,
    EIteExt: ξ<X>,
    EFlipExt: ξ<X>,
    EObserveExt: ξ<X>,
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,

    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Clone,
    <AVarExt<SVal> as ξ<X>>::Ext: Clone,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AValExt<EVal> as ξ<X>>::Ext: Clone,
    <AValExt<SVal> as ξ<X>>::Ext: Clone,

    <EAnfExt as ξ<X>>::Ext: Clone,
    <EPrjExt as ξ<X>>::Ext: Clone,
    <EProdExt as ξ<X>>::Ext: Clone,
    <ELetInExt as ξ<X>>::Ext: Clone,
    <EIteExt as ξ<X>>::Ext: Clone,
    <EFlipExt as ξ<X>>::Ext: Clone,
    <EObserveExt as ξ<X>>::Ext: Clone,
    <SObserveExt as ξ<X>>::Ext: Clone,
    <ESampleExt as ξ<X>>::Ext: Clone,

    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Clone,
    <SLetInExt as ξ<X>>::Ext: Clone,
    <SSeqExt as ξ<X>>::Ext: Clone,
    <SIteExt as ξ<X>>::Ext: Clone,
    <SExactExt as ξ<X>>::Ext: Clone,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Clone,
    <SDiscreteExt as ξ<X>>::Ext: Clone,
    <SUniformExt as ξ<X>>::Ext: Clone,
    <SNormalExt as ξ<X>>::Ext: Clone,
    <SBetaExt as ξ<X>>::Ext: Clone,
    <SDirichletExt as ξ<X>>::Ext: Clone,
{
    fn clone(&self) -> Self {
        use Program::*;
        match self {
            SBody(e) => SBody(e.clone()),
            EBody(e) => EBody(e.clone()),
        }
    }
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
    SObserveExt: ξ<X>,
    ESampleExt: ξ<X>,
    <EAnfExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EPrjExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EProdExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ELetInExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EIteExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EFlipExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <EObserveExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SObserveExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <ESampleExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    AVarExt<EVal>: ξ<X>,
    AVarExt<SVal>: ξ<X>,
    AValExt<EVal>: ξ<X>,
    AValExt<SVal>: ξ<X>,
    <AVarExt<EVal> as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <AVarExt<SVal> as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <AValExt<EVal> as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <AValExt<SVal> as ξ<X>>::Ext: Debug + Clone + PartialEq,
    SAnfExt: ξ<X>,
    SLetInExt: ξ<X>,
    SSeqExt: ξ<X>,
    SIteExt: ξ<X>,
    SExactExt: ξ<X>,
    <SAnfExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SLetInExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SSeqExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SIteExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SExactExt as ξ<X>>::Ext: Debug + Clone + PartialEq,

    SBernExt: ξ<X>,
    SDiscreteExt: ξ<X>,
    SUniformExt: ξ<X>,
    SNormalExt: ξ<X>,
    SBetaExt: ξ<X>,
    SDirichletExt: ξ<X>,

    <SBernExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SDiscreteExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SUniformExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SNormalExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SBetaExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
    <SDirichletExt as ξ<X>>::Ext: Debug + Clone + PartialEq,
{
    pub fn query(&self) -> Query<X> {
        use Program::*;
        match self {
            EBody(e) => Query::EQuery(e.query()),
            SBody(e) => Query::SQuery(e.query()),
        }
    }
    pub fn insert_observe(&self, e: EExpr<X>) -> Program<X> {
        use Program::*;
        match self {
            EBody(b) => EBody(b.insert_observe(e)),
            SBody(b) => todo!(), //  Body(b.insert_observe(e)),
        }
    }
}

// UD comprises of the default, undecorated grammar. Grammar.rs seems to be the best place for this.
pub struct UD;
impl ξ<UD> for AVarExt<SVal> {
    type Ext = ();
}
impl ξ<UD> for AVarExt<EVal> {
    type Ext = ();
}
impl ξ<UD> for AValExt<EVal> {
    type Ext = ();
}
impl ξ<UD> for AValExt<SVal> {
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
impl ξ<UD> for SObserveExt {
    type Ext = ();
}
impl ξ<UD> for ESampleExt {
    type Ext = ();
}
impl ξ<UD> for SAnfExt {
    type Ext = ();
}
impl ξ<UD> for SLetInExt {
    type Ext = ();
}
impl ξ<UD> for SSeqExt {
    type Ext = ();
}
impl ξ<UD> for SIteExt {
    type Ext = ();
}
impl ξ<UD> for SBernExt {
    type Ext = ();
}
impl ξ<UD> for SDiscreteExt {
    type Ext = ();
}
impl ξ<UD> for SUniformExt {
    type Ext = ();
}
impl ξ<UD> for SNormalExt {
    type Ext = ();
}
impl ξ<UD> for SBetaExt {
    type Ext = ();
}
impl ξ<UD> for SDirichletExt {
    type Ext = ();
}

impl ξ<UD> for SExactExt {
    type Ext = ();
}

pub type AnfUD<X> = Anf<UD, X>;
pub type EExprUD = EExpr<UD>;
pub type SExprUD = SExpr<UD>;
pub type ProgramUD = Program<UD>;
