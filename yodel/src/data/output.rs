use crate::annotate::grammar::Var;
use crate::data::context::Context;
// use crate::data::importance::{Importance, I};
use crate::data::{Weight, WeightMap};
use crate::grammar::SVal;
use crate::uniquify::grammar::UniqueId;
use itertools::izip;
use num_traits::identities::Zero;
use rsdd::repr::bdd::BddPtr;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;

pub type SubstMap = HashMap<UniqueId, (Vec<BddPtr>, Var)>;

#[derive(Debug, Clone, PartialEq)]
pub struct Output {
    /// compiled distributions
    pub dists: Vec<BddPtr>,
    /// acceptance criteria
    pub accept: BddPtr,
    /// sample consistency
    pub samples: BddPtr,
    /// (unused, unnormalized) probabilities of the compiled distributions
    pub probabilities: Vec<Probability>, // FIXME: the more I think about this the more I think this is fundamentally broken in the presence of observe statements.
    /// compiled weightmap
    pub weightmap: WeightMap,
    /// substitution environment
    pub substitutions: SubstMap,
    /// sampling substitutions
    pub ssubstitutions: HashMap<UniqueId, Vec<SVal>>,
    /// sampling value
    pub sout: Vec<SVal>,
    // /// compiled importance weight
    // pub importance: Importance,
}

impl Output {
    pub fn from_anf_dists(ctx: &Context, dists: Vec<BddPtr>) -> Output {
        let len = dists.len();
        let probabilities = vec![Probability::new(1.0); len];
        Output {
            dists,
            accept: ctx.accept,
            samples: ctx.samples,
            substitutions: ctx.substitutions.clone(),
            weightmap: ctx.weightmap.clone(),
            probabilities,
            // importance: I::Weight(1.0),
            ssubstitutions: ctx.ssubstitutions.clone(),
            sout: vec![],
        }
    }
    // pub fn for_sample_lang_sample(ctx: &Context, sample: bool, dist: BddPtr) -> Output {
    //     let newsample = if sample { dist } else { dist.not() };
    //     Output {
    //         dists: vec![],
    //         accept: ctx.accept,
    //         samples: ctx.samples,

    //         // pass through
    //         substitutions: ctx.substitutions.clone(),
    //         weightmap: ctx.weightmap.clone(),

    //         // unused
    //         probabilities: vec![Probability::new(1.0)],
    //         importance: I::Weight(1.0),
    //     }
    // }

    pub fn for_sample_lang(ctx: &Context) -> Output {
        // let accept = BddPtr::from_bool(sample.unwrap_or_else(|| false));
        Output {
            dists: vec![BddPtr::PtrTrue],
            accept: ctx.accept,
            samples: ctx.samples,

            // pass through
            substitutions: ctx.substitutions.clone(),
            weightmap: ctx.weightmap.clone(),

            // unused
            probabilities: vec![Probability::new(1.0)],
            // importance: I::Weight(1.0),
            ssubstitutions: ctx.ssubstitutions.clone(),
            sout: vec![],
        }
    }
    pub fn sval(&self) -> Vec<SVal> {
        self.sout.clone()
    }
    pub fn sint(&self) -> u64 {
        SVal::vec_as(&self.sout, &SVal::as_int)
    }
    pub fn sbool(&self) -> bool {
        SVal::vec_as(&self.sout, &SVal::as_bool)
    }
    pub fn sfloat(&self) -> f64 {
        SVal::vec_as(&self.sout, &SVal::as_float)
    }
    pub fn sfloats(&self) -> Vec<f64> {
        self.sval().iter().map(|v| v.as_float()).collect()
    }
}

