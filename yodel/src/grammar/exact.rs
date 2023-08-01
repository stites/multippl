use crate::desugar::exact::integers;
use crate::*;
use ::core::fmt;
use ::core::fmt::Debug;
use itertools::Itertools;
use rsdd::builder::bdd_plan::BddPlan;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::string::String;

use super::anf::*;
use super::sampling::SExpr;
use super::ttg::*;

// pub trait NaturalEmbedding<Fr: Lang, To: Lang> {
//     fn is_embedable(_: <Fr as Lang>::Val) -> bool;
//     fn embed(_: <Fr as Lang>::Val) -> Option<<To as Lang>::Val>;
//     fn embed_type(_: <Fr as Lang>::Ty) -> Option<<Fr as Lang>::Ty>;
// }

crate::TTG!(
    impl<X> Lang for EExpr<X> {
        type Ty = ETy;
        type Val = EVal;
        type Function = super::function::Function<EExpr<X>>;
        type Anf = super::anf::Anf<X, EVal>;
    }
);

crate::TTG!(
    impl<X> NaturalEmbedding<SExpr<X>> for EExpr<X> {
        type Val = EVal;
        fn embed(e: &SVal) -> Result<EVal> {
            use EVal::*;
            use SVal::*;
            match e {
                SBool(b) => Ok(EBdd(BddPlan::from_bool(*b))),
                SFloat(f) => Ok(EFloat(*f)),
                // SInt(i) => Ok(integers::as_onehot(*i as usize)),
                SInt(i) => Ok(EInteger(*i as usize)),
                SVec(vs) => Ok(EProd(
                    vs.iter().map(Self::embed).collect::<Result<Vec<_>>>()?,
                )),
                SProd(vs) => Ok(EProd(
                    vs.iter().map(Self::embed).collect::<Result<Vec<_>>>()?,
                )),
                SDist(_) => {
                    errors::generic("dist in the MLS boundary: did you forget to sample using `~`?")
                }
            }
        }
    }
);

#[derive(Debug, Clone, PartialEq)]
pub enum ETy {
    EBool,
    EFloat,
    EFormula,
    EProd(Vec<ETy>),
    EInt,
}
impl ETy {
    pub fn right(&self) -> Option<ETy> {
        match self {
            ETy::EProd(tys) => match &tys[..] {
                [_, r] => Some(r.clone()),
                _ => None,
            },
            _ => None,
        }
    }
    pub fn left(&self) -> Option<ETy> {
        match self {
            ETy::EProd(tys) => match &tys[..] {
                [l, _] => Some(l.clone()),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EVal {
    // EBool(bool),
    EBdd(BddPlan),
    EFloat(f64),
    EProd(Vec<EVal>),
    // extensions: not part of the core IR, just included in the maximal values as part of TTG
    EInteger(usize),
}
impl EVal {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::EBdd(BddPlan::ConstTrue) => Some(true),
            Self::EBdd(BddPlan::ConstFalse) => Some(false),
            _ => None,
        }
    }
    // pub fn is_true(&self) -> bool {
    //     use EVal::*;
    //     match self {
    //         EBdd(BddPlan::ConstTrue) => true,
    //         _ => false,
    //     }
    // }
}

impl super::classes::IsTyped<ETy> for EVal {
    fn is_prod(&self) -> bool {
        matches!(self, EVal::EProd(_))
    }
    fn as_type(&self) -> ETy {
        use EVal::*;
        match self {
            // EBool(_) => ETy::EBool,
            EFloat(_) => ETy::EFloat,
            EProd(vs) => ETy::EProd(vs.iter().map(|x| x.as_type()).collect_vec()),
            EBdd(BddPlan::ConstTrue) => ETy::EBool,
            EBdd(BddPlan::ConstFalse) => ETy::EBool,
            EBdd(_) => ETy::EFormula,
            EInteger(i) => ETy::EProd(vec![ETy::EBool; *i]),
        }
    }
}

TTG!(
    #[derive(Debug, Clone, PartialEq)]
    pub enum EExpr<X> {
        EAnf(<SAnfExt as ξ<X>>::Ext, Box<Anf<X, EVal>>),

        // EPrj(<EPrjExt as ξ<X>>::Ext, Box<Anf<X, EVal>>, Box<Anf<X, EVal>>),
        // EProd(<EProdExt as ξ<X>>::Ext, Vec<Anf<X, EVal>>),
        EApp(<EAppExt as ξ<X>>::Ext, String, Vec<Anf<X, EVal>>),
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
        EFlip(<EFlipExt as ξ<X>>::Ext, Box<Anf<X, EVal>>),
        EObserve(<EObserveExt as ξ<X>>::Ext, Box<Anf<X, EVal>>),
        ESample(<ESampleExt as ξ<X>>::Ext, Box<SExpr<X>>),

        // sugar: integer support
        EDiscrete(<EDiscreteExt as ξ<X>>::Ext, Vec<Anf<X, EVal>>), // => if-then-else chain returning a one-hot encoding
        // sugar: iterate(f, init, k)
        EIterate(
            <EIterateExt as ξ<X>>::Ext,
            String,            // function name
            Box<Anf<X, EVal>>, // initial argument
            Box<Anf<X, EVal>>, // number of times to iterate
        ),
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
