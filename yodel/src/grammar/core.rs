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

use super::ttg::*;

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

#[derive(Debug, PartialEq, Clone)]
pub enum Anf<X, Val>
where
    AVarExt<Val>: ξ<X>,
    AValExt<Val>: ξ<X>,
    <AVarExt<Val> as ξ<X>>::Ext: Debug + PartialEq + Clone,
    <AValExt<Val> as ξ<X>>::Ext: Debug + PartialEq + Clone,
    Val: Debug + PartialEq + Clone,
    X: Debug + PartialEq + Clone,
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
// pub trait DPC: Debug + PartialEq + Clone {}
// impl<X: DPC, Val: DPC> DPC for Anf<X, Val>
// where
//     AVarExt<Val>: ξ<X>,
//     AValExt<Val>: ξ<X>,
//     <AVarExt<Val> as ξ<X>>::Ext: DPC,
//     <AValExt<Val> as ξ<X>>::Ext: DPC,
//     X: DPC,
//     Val: DPC,
// {
// }

TTG!(
    #[derive(Debug, Clone, PartialEq)]
    pub enum EExpr<X> {
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
);

#[derive(Debug, Clone, PartialEq)]
pub enum SVal {
    SBool(bool),
    SFloat(f64),
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

TTG!(
    #[derive(Debug, Clone, PartialEq)]
    pub enum SExpr<X> {
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
);

TTG!(
    impl<X> EExpr<X> {
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
);

TTG!(
    impl<X> SExpr<X> {
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
);

TTG!(
    #[derive(Debug, Clone, PartialEq)]
    pub enum Program<X> {
        EBody(EExpr<X>),
        SBody(SExpr<X>),
        // TODO
        // | define (f: Func) (rest: Program) : Program
    }
);

TTG!(
    #[derive(Debug, Clone, PartialEq)]
    pub enum Query<X> {
        EQuery(EExpr<X>),
        SQuery(SExpr<X>),
    }
);

TTG!(
    impl<X> Program<X> {
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
);

// UD comprises of the default, undecorated grammar.
// all extensions are of type ().
ttg::phase!(pub struct UD);
ttg::alias!(UD + (Program, EExpr, SExpr, Anf<Var>));
