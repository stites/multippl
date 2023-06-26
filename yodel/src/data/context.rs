use crate::annotate::grammar::Var;
use crate::data::output::{Output, SubstMap};
use crate::data::WeightMap;
use crate::grammar::SVal;
use crate::uniquify::grammar::UniqueId;
use rsdd::repr::bdd::BddPtr;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Context {
    pub accept: BddPtr,
    pub samples: BddPtr,
    pub substitutions: SubstMap,
    pub weightmap: WeightMap,
    pub ssubstitutions: HashMap<UniqueId, Vec<SVal>>,
}
impl Context {
    pub fn from_compiled(c: &Output) -> Self {
        Context {
            accept: c.accept,
            samples: c.samples,
            substitutions: c.substitutions.clone(),
            weightmap: c.weightmap.clone(),
            ssubstitutions: c.ssubstitutions.clone(),
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
            ssubstitutions: Default::default(),
        }
    }
}
