use crate::compile::{Compiled, SubstMap, WeightMap};
use rsdd::repr::bdd::BddPtr;

#[derive(Debug, Clone)]
pub struct Context {
    pub accept: BddPtr,
    pub substitutions: SubstMap,
    pub weightmap: WeightMap,
}
impl Context {
    pub fn from_compiled(c: &Compiled) -> Self {
        Context {
            accept: c.accept.clone(),
            substitutions: c.substitutions.clone(),
            weightmap: c.weightmap.clone(),
        }
    }
}
impl Default for Context {
    fn default() -> Self {
        Context {
            accept: BddPtr::PtrTrue,
            substitutions: Default::default(),
            weightmap: Default::default(),
        }
    }
}
