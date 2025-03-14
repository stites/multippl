use super::ttg::*;
use std::fmt::Debug;

#[derive(Debug, PartialEq, Clone)]
pub enum Anf<X, Val>
where
    AVarExt<Val>: ξ<X>,
    AValExt<Val>: ξ<X>,
    // APrjExt<Val>: ξ<X>,
    ADistExt<Val>: ξ<X>,
    <AVarExt<Val> as ξ<X>>::Ext: Debug + PartialEq + Clone,
    // <APrjExt<Val> as ξ<X>>::Ext: Debug + PartialEq + Clone,
    <AValExt<Val> as ξ<X>>::Ext: Debug + PartialEq + Clone,
    <ADistExt<Val> as ξ<X>>::Ext: Debug + PartialEq + Clone,
    Val: Debug + PartialEq + Clone,
    X: Debug + PartialEq + Clone,
{
    AVar(<AVarExt<Val> as ξ<X>>::Ext, String),
    AVal(<AValExt<Val> as ξ<X>>::Ext, Val),

    // Boolean ops
    And(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    Or(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    Xor(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
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

    // [x] && x [ 0 ]
    AnfVec(Vec<Anf<X, Val>>), // in the exact language, this denotes a one-hot encoded integer
    AnfPush(Box<Anf<X, Val>>, Box<Anf<X, Val>>),
    AnfHead(Box<Anf<X, Val>>),
    AnfTail(Box<Anf<X, Val>>),

    AnfTrace(Box<Anf<X, Val>>, Box<Anf<X, Val>>),

    AnfProd(Vec<Anf<X, Val>>),
    AnfPrj(
        Box<Anf<X, Val>>, // tuple to index
        Box<Anf<X, Val>>,
    ), // index value

    // Distributions
    AnfBernoulli(<ADistExt<Val> as ξ<X>>::Ext, Box<Anf<X, Val>>),
    AnfDiscrete(<ADistExt<Val> as ξ<X>>::Ext, Vec<Anf<X, Val>>),
    AnfBinomial(
        <ADistExt<Val> as ξ<X>>::Ext,
        Box<Anf<X, Val>>,
        Box<Anf<X, Val>>,
    ),
    AnfUniform(
        <ADistExt<Val> as ξ<X>>::Ext,
        Box<Anf<X, Val>>,
        Box<Anf<X, Val>>,
    ),
    AnfNormal(
        <ADistExt<Val> as ξ<X>>::Ext,
        Box<Anf<X, Val>>,
        Box<Anf<X, Val>>,
    ),
    AnfPoisson(<ADistExt<Val> as ξ<X>>::Ext, Box<Anf<X, Val>>),
    AnfBeta(
        <ADistExt<Val> as ξ<X>>::Ext,
        Box<Anf<X, Val>>,
        Box<Anf<X, Val>>,
    ),
    AnfDirichlet(<ADistExt<Val> as ξ<X>>::Ext, Vec<Anf<X, Val>>),
}
