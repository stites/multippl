use crate::annotate::grammar::*;
use crate::grammar::*;
use crate::uniquify::grammar::UniqueId;
use crate::utils::render::*;
use crate::*;
// use itertools::*;
// use num_traits::*;
// use rand::distributions::{Bernoulli, Distribution};
// use rand::rngs::StdRng;
// use rand::SeedableRng;
// use rsdd::builder::bdd_builder::BddManager;
// use rsdd::builder::cache::all_app::AllTable;
// use rsdd::builder::cache::*;
// use rsdd::repr::bdd::*;
// use rsdd::repr::ddnnf::*;
// use rsdd::repr::var_label::*;
// use rsdd::repr::var_order::VarOrder;
// use rsdd::repr::wmc::*;
// use rsdd::sample::probability::Probability;
// use std::collections::HashMap;
// use std::collections::HashSet;
// use std::fmt;
// use std::string::String;
// use tracing::*;
// use either::{Either, Either::*};

pub use crate::data::context::Context;
pub use crate::data::errors::{CompileError, Result};
pub use crate::data::importance::{Importance, I};
pub use crate::data::output::{Compiled, Output, SubstMap};
pub use crate::data::{Weight, WeightMap};
use CompileError::*;

use std::fmt::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Trace;

impl ξ<Trace> for AVarExt<EVal> {
    type Ext = Box<Output>;
}
impl ξ<Trace> for AValExt<EVal> {
    type Ext = Box<Output>;
}
impl ξ<Trace> for AVarExt<SVal> {
    type Ext = Box<Output>;
}
impl ξ<Trace> for AValExt<SVal> {
    type Ext = Box<Output>;
}
pub type AnfTr<Val> = Anf<Trace, Val>;

impl ξ<Trace> for EAnfExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for EPrjExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for EProdExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for ELetInExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for EIteExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for EFlipExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for EObserveExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for ESampleExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for SAnfExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for SLetInExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for SSeqExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for SIteExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for SFlipExt {
    type Ext = Box<Compiled>;
}
impl ξ<Trace> for SExactExt {
    type Ext = Box<Compiled>;
}

pub type EExprTr = EExpr<Trace>;
pub type SExprTr = SExpr<Trace>;
pub type ProgramTr = Program<Trace>;
