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

use super::classes::*;
use super::ttg::*;

TTG!(
    impl<X> Lang for SExpr<X> {
        type Ty = STy;
        type Val = SVal;
        type Function = super::function::Function<SExpr<X>>;
        type Anf = super::anf::Anf<X, SVal>;
    }
);

#[derive(Debug, Clone, PartialEq)]
pub enum SVal {
    SBool(bool),
    SFloat(f64),
    SInt(u64),
    SVec(Vec<SVal>),
    SProd(Vec<SVal>),

    SBern(f64),
    SDiscrete(Vec<f64>),
    SUniform(f64, f64),
    SNormal(f64, f64),
    SPoisson(f64),
    SBeta(f64, f64),
    SDirichlet(Vec<f64>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum STy {
    SBool,
    SFloat,
    SInt,
    SVec(Box<STy>),
    SProd(Vec<STy>),
    SDistribution,
}

impl super::classes::IsTyped<STy> for SVal {
    fn is_prod(&self) -> bool {
        matches!(self, SVal::SVec(_))
    }
    fn as_type(&self) -> STy {
        use SVal::*;
        match self {
            SBool(_) => STy::SBool,
            SFloat(_) => STy::SFloat,
            SInt(_) => STy::SInt,
            SVec(vs) => STy::SVec(Box::new(vs.first().unwrap().as_type())),
            SProd(vs) => STy::SProd(vs.iter().map(|x| x.as_type()).collect()),
            _ => STy::SDistribution,
        }
    }
}
impl SVal {
    // pub fn as_query(&self) -> f64 {
    //     use SVal::*;
    //     match self {
    //         SBool(x) => {
    //             if *x {
    //                 1.0
    //             } else {
    //                 0.0
    //             }
    //         }
    //         SInt(x) => *x as f64,
    //         SFloat(x) => *x,
    //     }
    // }
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
        SMap(
            <SMapExt as ξ<X>>::Ext,
            String,            // argument
            Box<SExpr<X>>,     // map function body
            Box<Anf<X, SVal>>, // value to fold
        ),
        SFold(
            <SFoldExt as ξ<X>>::Ext,
            Box<Anf<X, SVal>>, // initial value
            String,            // accumulator
            String,            // argument
            Box<SExpr<X>>,     // fold function body
            Box<Anf<X, SVal>>, // value to fold
        ),
        SWhile(
            <SWhileExt as ξ<X>>::Ext,
            Box<Anf<X, SVal>>, // guard
            Box<SExpr<X>>,     // body
        ),
        SApp(<SAnfExt as ξ<X>>::Ext, String, Vec<Anf<X, SVal>>),
        SLambda(<SLambdaExt as ξ<X>>::Ext, Vec<String>, Box<SExpr<X>>),

        SSample(<SSampleExt as ξ<X>>::Ext, Box<Anf<X, SVal>>),
        SObserve(
            <SObserveExt as ξ<X>>::Ext,
            Box<Anf<X, SVal>>,
            Box<Anf<X, SVal>>,
        ),

        // Multi-language boundary
        SExact(<SExactExt as ξ<X>>::Ext, Box<EExpr<X>>),

        // sugar: let x = ~(<dist anf>) in <sexpr>
        SLetSample(
            <SLetSampleExt as ξ<X>>::Ext,
            String,
            Box<Anf<X, SVal>>,
            Box<SExpr<X>>,
        ),
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
