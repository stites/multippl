use crate::annotate::grammar::Var;
use crate::compile::context::Context;
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
pub struct Output {
    /// compiled distributions
    pub dists: Vec<BddPtr>,
    /// acceptance criteria
    pub accept: BddPtr,
    /// sample consistency
    pub samples: BddPtr,
    /// (unused, unnormalized) probabilities of the compiled distributions
    pub probabilities: Vec<Probability>,
    /// compiled weightmap
    pub weightmap: WeightMap,
    /// substitution environment
    pub substitutions: SubstMap,
    /// compiled importance weight
    pub importance: Importance,
}

impl Output {
    pub fn from_anf_dists(ctx: &Context, dists: Vec<BddPtr>) -> Output {
        let probabilities = vec![Probability::new(1.0); dists.len()];
        Output {
            dists,
            accept: ctx.accept,
            samples: ctx.samples,
            substitutions: ctx.substitutions.clone(),
            weightmap: ctx.weightmap.clone(),
            probabilities,
            importance: I::Weight(1.0),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Compiled {
    Output(Output),
    Debug(Vec<Output>),
}
impl Compiled {
    pub fn is_single_run(&self) -> bool {
        matches!(self, Compiled::Output(_))
    }
    pub fn from_output(o: Output) -> Compiled {
        Compiled::Output(o)
    }
    pub fn as_output(&self) -> Option<Output> {
        match self {
            Compiled::Output(o) => Some(o.clone()),
            Compiled::Debug(_) => None,
        }
    }
}
impl IntoIterator for Compiled {
    type Item = Output;
    type IntoIter = std::vec::IntoIter<Output>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Compiled::Output(o) => vec![o].into_iter(),
            Compiled::Debug(os) => os.into_iter(),
        }
    }
}

impl FromIterator<Output> for Compiled {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Output>,
    {
        let os = iter.into_iter().collect::<Vec<Output>>();
        let os_len = os.len();
        match os_len {
            0 => panic!("empty iterator, cannot produce compiled output"),
            1 => Compiled::from_output(os[0].clone()),
            _ => Compiled::Debug(os),
        }
    }
}
