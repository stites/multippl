use super::ttg::*;
use std::fmt::Debug;

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

    // Numerics
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

    // Distributions
    EFlip(Box<Anf<X, Val>>),
    SBern(Box<Anf<X, Val>>),
    SDiscrete(Vec<Anf<X, Val>>),
    SUniform(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    SNormal(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    SPoisson(Box<Anf<X, Val>>),
    SBeta(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    SDirichlet(Vec<Anf<X, Val>>),
}
