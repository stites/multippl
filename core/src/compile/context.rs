use crate::annotate::grammar::Var;
use crate::compile::{Output, SubstMap, WeightMap};
use rsdd::repr::bdd::BddPtr;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Context {
    pub accept: BddPtr,
    pub samples: BddPtr,
    pub substitutions: SubstMap,
    pub weightmap: WeightMap,
}
impl Context {
    pub fn from_compiled(c: &Output) -> Self {
        Context {
            accept: c.accept,
            samples: c.samples,
            substitutions: c.substitutions.clone(),
            weightmap: c.weightmap.clone(),
        }
    }
}
impl Default for Context {
    fn default() -> Self {
        Context {
            accept: BddPtr::PtrTrue,
            samples: BddPtr::PtrTrue,
            substitutions: Default::default(),
            weightmap: Default::default(),
        }
    }
}
