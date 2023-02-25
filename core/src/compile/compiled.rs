use crate::annotate::grammar::Var;
use crate::compile::importance::{Importance, I};
use crate::compile::weighting::{Weight, WeightMap};
use crate::uniquify::grammar::UniqueId;
use itertools::izip;
use num_traits::identities::Zero;
use rsdd::repr::bdd::BddPtr;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;

pub type SubstMap = HashMap<UniqueId, (Vec<BddPtr>, Var)>;

#[derive(Debug, Clone)]
pub struct Compiled {
    pub dists: Vec<BddPtr>,
    pub accept: BddPtr,
    pub probabilities: Vec<Probability>,
    pub weightmap: WeightMap,
    pub substitutions: SubstMap,
    pub importance: Importance,
}
impl Compiled {
    pub fn convex_combination(&self, o: &Compiled) -> Importance {
        izip!(&self.probabilities, &o.probabilities,).fold(Zero::zero(), |res, (selfp, op)| {
            I::Weight(
                (selfp.as_f64() * self.importance.weight() + op.as_f64() * o.importance.weight())
                    / 2.0,
            )
        })
    }
}
