use crate::data::HashMap;
/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
use crate::*;
use core::fmt;
use core::fmt::Debug;
use itertools::Itertools;
use rsdd::builder::bdd_plan::BddPlan;
use std::any::{Any, TypeId};
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

crate::TTG!(
    impl<X> NaturalEmbedding<EExpr<X>> for SExpr<X> {
        type Val = SVal;
        fn embed(e: &EVal) -> Result<SVal> {
            use EVal::*;
            use SVal::*;
            match e {
                EBdd(BddPtr::PtrTrue) => Ok(SBool(true)),
                EBdd(BddPtr::PtrFalse) => Ok(SBool(false)),
                EFloat(f) => Ok(SFloat(*f)),
                EProd(vs) => Ok(SProd(
                    vs.iter().map(Self::embed).collect::<Result<Vec<_>>>()?,
                )),
                EInteger(i) => Ok(SInt(*i as u64)),
                EBdd(_) => errors::semantics("sampling lang cannot compile a formula indirectly"),
            }
        }
    }
);

#[derive(Debug, Clone, PartialEq)]
pub enum Dist {
    Bern(f64),
    Discrete(Vec<f64>),
    Uniform(f64, f64),
    Normal(f64, f64),
    Poisson(f64),
    Beta(f64, f64),
    Dirichlet(Vec<f64>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SVal {
    SBool(bool),
    SFloat(f64),
    SInt(u64),
    SVec(Vec<SVal>),
    SProd(Vec<SVal>),
    SDist(Dist),
}
impl SVal {
    pub fn pretty(&self) -> String {
        match self {
            SVal::SBool(x) => format!("{}", x),
            SVal::SFloat(x) => format!("{}", x),
            SVal::SInt(x) => format!("{}", x),
            SVal::SVec(xs) => format!("[{}]", xs.iter().map(Self::pretty).collect_vec().join(", ")),
            SVal::SProd(xs) => {
                format!("({})", xs.iter().map(Self::pretty).collect_vec().join(", "))
            }
            SVal::SDist(x) => format!("{:?}", x),
        }
    }
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
impl From<&SVal> for Vec<f64> {
    fn from(s: &SVal) -> Vec<f64> {
        use SVal::*;
        match s {
            SBool(x) => {
                if *x {
                    vec![1.0]
                } else {
                    vec![0.0]
                }
            }
            SInt(x) => vec![*x as f64],
            SFloat(x) => vec![*x],
            SProd(xs) => xs.iter().fold(vec![], |mut acc, x| {
                let xs: Vec<f64> = From::from(x);
                acc.extend(xs);
                acc
            }),
            SVec(xs) => xs.iter().fold(vec![], |mut acc, x| {
                let xs: Vec<f64> = From::from(x);
                acc.extend(xs);
                acc
            }),
            SDist(_) => vec![],
        }
    }
}
impl SVal {
    pub fn from_bools(bs: &[bool]) -> Vec<Self> {
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
        SApp(<SAppExt as ξ<X>>::Ext, String, Vec<Anf<X, SVal>>),
        SLambda(<SLambdaExt as ξ<X>>::Ext, Vec<String>, Box<SExpr<X>>),

        SSample(<SSampleExt as ξ<X>>::Ext, Box<SExpr<X>>),
        SObserve(
            <SObserveExt as ξ<X>>::Ext,
            Box<Anf<X, SVal>>, // value
            Box<Anf<X, SVal>>, // in distribution (also a value)
            Box<SExpr<X>>,     // for <rest of the program>
        ),

        // Multi-language boundary
        SExact(<SExactExt as ξ<X>>::Ext, Box<EExpr<X>>),

        // sugar: let x = ~(<dist anf>) in <sexpr>
        SLetSample(
            <SLetSampleExt as ξ<X>>::Ext,
            String,        // variable bound to sample
            Box<SExpr<X>>, // distribution to sample
            Box<SExpr<X>>, // rest of binding
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
