use crate::annotate::grammar::Var;
// use crate::data::context::Context;
// use crate::data::importance::{Importance, I};
use crate::data::{Weight, WeightMap};
use crate::grammar::{EVal, SVal};
use crate::uniquify::grammar::UniqueId;
use crate::Dist;
use itertools::izip;
use num_traits::identities::Zero;
use rsdd::builder::bdd_plan::BddPlan;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;

pub type SubstMap = HashMap<UniqueId, (Vec<EVal>, Var)>;

/// exact compilation context
#[derive(Debug, Clone, PartialEq)]
pub struct ECtx {
    pub accept: BddPlan,
    pub samples: Vec<(BddPlan, bool)>,
    pub substitutions: SubstMap,
    pub weightmap: WeightMap,
}
impl Default for ECtx {
    fn default() -> Self {
        ECtx {
            accept: BddPlan::ConstTrue,
            samples: Default::default(),
            substitutions: Default::default(),
            weightmap: Default::default(),
        }
    }
}
impl ECtx {
    pub fn from_output(out: &EOutput) -> Self {
        ECtx {
            accept: out.accept.clone(),
            samples: out.samples.clone(),
            substitutions: out.substitutions.clone(),
            weightmap: out.weightmap.clone(),
        }
    }
    pub fn as_output(&self, out: Vec<EVal>) -> EOutput {
        EOutput {
            out,
            accept: self.accept.clone(),
            samples: self.samples.clone(),
            substitutions: self.substitutions.clone(),
            weightmap: self.weightmap.clone(),
        }
    }
}

/// exact compilation output
#[derive(Debug, Clone, PartialEq)]
pub struct EOutput {
    /// compiled distributions
    pub out: Vec<EVal>,
    /// acceptance criteria
    pub accept: BddPlan,
    /// sample consistency
    pub samples: Vec<(BddPlan, bool)>,
    /// compiled weightmap
    pub weightmap: WeightMap,
    /// substitution environment
    pub substitutions: SubstMap,
}
impl EOutput {
    pub fn from_anf_out(ctx: &ECtx, out: Vec<EVal>) -> Self {
        EOutput {
            out,
            accept: ctx.accept.clone(),
            samples: ctx.samples.clone(),
            substitutions: ctx.substitutions.clone(),
            weightmap: ctx.weightmap.clone(),
        }
    }
    pub fn pkg(&self) -> Box<Output> {
        Box::new(Output::Exact(self.clone()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SCtx {
    pub substitutions: HashMap<UniqueId, Vec<SVal>>,
    pub trace: HashMap<UniqueId, (SVal, Dist, Probability)>,
}
impl Default for SCtx {
    fn default() -> Self {
        SCtx {
            substitutions: Default::default(),
            trace: Default::default(),
        }
    }
}
impl SCtx {
    pub fn from_output(out: &SOutput) -> Self {
        SCtx {
            substitutions: out.substitutions.clone(),
            trace: out.trace.clone(),
        }
    }
    pub fn as_output(&self, out: Vec<SVal>) -> SOutput {
        SOutput {
            out,
            trace: self.trace.clone(),
            substitutions: self.substitutions.clone(),
        }
    }
}
#[derive(Default, Debug, Clone, PartialEq)]
pub struct SOutput {
    /// compiled output
    pub out: Vec<SVal>,
    /// sampling substitutions
    pub substitutions: HashMap<UniqueId, Vec<SVal>>,
    // sampled values traces (and ids)
    pub trace: HashMap<UniqueId, (SVal, Dist, Probability)>,
}
impl SOutput {
    pub fn head(&self) -> SVal {
        self.out[0].clone()
    }
    pub fn pkg(&self) -> Box<Output> {
        Box::new(Output::Sample(self.clone()))
    }
}

/// interop output
#[derive(Debug, Clone, PartialEq)]
pub struct IOutput {
    pub exact: EOutput,
    pub sample: SOutput,
    // /// compiled importance weight
    // pub importance: Importance,
    // pub interop: (),
    // /// compiled importance weight
    // pub importance: Importance,
}
/// interop context
#[derive(Debug, Clone, PartialEq)]
pub struct ICtx {
    pub exact: ECtx,
    pub sample: SCtx,
}
impl Default for ICtx {
    fn default() -> Self {
        ICtx {
            exact: Default::default(),
            sample: Default::default(),
        }
    }
}

/// whole-program output, which can be one of: exact compilation, sampling
/// compilation, MLS semantics
#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    Exact(EOutput),
    Sample(SOutput),
    // Interop(IOutput),
}

/// whole-program context. initialized as empty
#[derive(Debug, Clone, PartialEq)]
pub enum Ctx {
    // Empty,
    Exact(ECtx),
    Sample(SCtx),
    // Interop(ICtx),
}
// impl Default for Ctx {
//     fn default() -> Self {
//         Ctx::Empty
//     }
// }
// impl Ctx {
//     pub fn from_output(c: &Output) -> Self {
//         use Ctx::*;
//         match c {
//             Output::Exact(out) => Exact(ECtx::from_output(out)),
//             Output::Sample(out) => Sample(SCtx::from_output(out)),
//             Output::Interop(out) => Interop(ICtx {
//                 sample: SCtx::from_output(&out.sample),
//                 exact: ECtx::from_output(&out.exact),
//             }),
//         }
//     }
// }
// impl Output {
//     // pub fn for_sample_lang_sample(ctx: &Context, sample: bool, dist: BddPtr) -> Output {
//     //     let newsample = if sample { dist } else { dist.not() };
//     //     Output {
//     //         out: vec![],
//     //         accept: ctx.accept,
//     //         samples: ctx.samples,

//     //         // pass through
//     //         substitutions: ctx.substitutions.clone(),
//     //         weightmap: ctx.weightmap.clone(),

//     //         // unused
//     //         probabilities: vec![Probability::new(1.0)],
//     //         importance: I::Weight(1.0),
//     //     }
//     // }

//     // pub fn for_sample_lang(ctx: &Context) -> Output {
//     //     // let accept = BddPtr::from_bool(sample.unwrap_or_else(|| false));
//     //     Output {
//     //         out: vec![BddPtr::PtrTrue],
//     //         accept: ctx.accept,
//     //         samples: ctx.samples,

//     //         // pass through
//     //         substitutions: ctx.substitutions.clone(),
//     //         weightmap: ctx.weightmap.clone(),

//     //         // unused
//     //         probabilities: vec![Probability::new(1.0)],
//     //         // importance: I::Weight(1.0),
//     //         ssubstitutions: ctx.ssubstitutions.clone(),
//     //         sout: vec![],
//     //     }
//     // }
//     // pub fn sval(&self) -> Vec<SVal> {
//     //     self.sout.clone()
//     // }
//     // pub fn sint(&self) -> u64 {
//     //     SVal::vec_as(&self.sout, &SVal::as_int)
//     // }
//     // pub fn sbool(&self) -> bool {
//     //     SVal::vec_as(&self.sout, &SVal::as_bool)
//     // }
//     // pub fn sfloat(&self) -> f64 {
//     //     SVal::vec_as(&self.sout, &SVal::as_float)
//     // }
//     // pub fn sfloats(&self) -> Vec<f64> {
//     //     self.sval().iter().map(|v| v.as_float()).collect()
//     // }
// }
