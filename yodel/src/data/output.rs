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

#[derive(Debug, Clone, PartialEq)]
pub enum Compiled {
    Output(Output),
    // Debug(Vec<Output>),
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
            //     Compiled::Debug(_) => None,
        }
    }
    pub fn unsafe_output(&self) -> Output {
        self.as_output().expect("debug mode should be disabled")
    }
    pub fn sval(&self) -> Vec<SVal> {
        match self {
            Compiled::Output(o) => o.sval(),
            //   Compiled::Debug(_) => panic!("typechecking says this is impossible!"),
        }
    }
}
// impl IntoIterator for Compiled {
//     type Item = Output;
//     type IntoIter = std::vec::IntoIter<Output>;

//     fn into_iter(self) -> Self::IntoIter {
//         match self {
//             Compiled::Output(o) => vec![o].into_iter(),
//             // Compiled::Debug(os) => os.into_iter(),
//         }
//     }
// }

// impl FromIterator<Output> for Compiled {
//     fn from_iter<T>(iter: T) -> Self
//     where
//         T: IntoIterator<Item = Output>,
//     {
//         let os = iter.into_iter().collect::<Vec<Output>>();
//         let os_len = os.len();
//         match os_len {
//             0 => panic!("empty iterator, cannot produce compiled output"),
//             1 => Compiled::from_output(os[0].clone()),
//             // _ => Compiled::Debug(os),
//             _ => todo!("debug mode"),
//         }
//     }
// }
