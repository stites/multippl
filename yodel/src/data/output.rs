use crate::annotate::grammar::Var;
// use crate::data::context::Context;
// use crate::data::importance::{Importance, I};
use crate::data::errors;
use crate::data::{Result, Weight, WeightMap};
use crate::grammar::{EVal, SVal};
use crate::uniquify::grammar::UniqueId;
use crate::Dist;
use itertools::izip;
use num_traits::identities::Zero;
use rsdd::builder::bdd_plan::BddPlan;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;

pub type SubstMap = HashMap<UniqueId, (Vec<EVal>, Var)>;
pub type Tr = Vec<(SVal, Dist, Probability, Option<UniqueId>)>;

/// exact compilation context
#[derive(Debug, Clone, PartialEq)]
pub struct ECtx {
    pub accept: BddPlan,
    pub samples: Vec<(BddPlan, bool)>,
    pub substitutions: SubstMap,
    pub weightmap: WeightMap,
}
pub trait GetSamples {
    /// optionally prune these samples.
    fn compile_samples(&self) -> BddPlan;
    fn samples(&self, prune: bool) -> BddPlan {
        if prune {
            BddPlan::ConstTrue
        } else {
            Self::compile_samples(self)
        }
    }
}

fn _all_samples(samples: &[(BddPlan, bool)]) -> BddPlan {
    samples.iter().fold(BddPlan::ConstTrue, |ss, (dist, s)| {
        BddPlan::and(ss, BddPlan::iff(dist.clone(), BddPlan::from_bool(*s)))
    })
}
impl GetSamples for ECtx {
    fn compile_samples(&self) -> BddPlan {
        _all_samples(&self.samples)
    }
}
impl GetSamples for Ctx {
    fn compile_samples(&self) -> BddPlan {
        GetSamples::compile_samples(&self.exact)
    }
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
impl From<&EOutput> for ECtx {
    fn from(out: &EOutput) -> Self {
        ECtx {
            accept: out.accept.clone(),
            samples: out.samples.clone(),
            substitutions: out.substitutions.clone(),
            weightmap: out.weightmap.clone(),
        }
    }
}
// impl From<&EOutput> for Ctx {
//     fn from(out: &EOutput) -> Self {

//         ECtx {
//             accept: out.accept.clone(),
//             samples: out.samples.clone(),
//             substitutions: out.substitutions.clone(),
//             weightmap: out.weightmap.clone(),
//         }
//     }
// }

impl ECtx {
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
impl GetSamples for EOutput {
    fn compile_samples(&self) -> BddPlan {
        _all_samples(&self.samples)
    }
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
    pub fn pkg(&self) -> Output {
        Output::exact(self.clone())
    }
    pub fn samples(&self) -> BddPlan {
        self.samples
            .iter()
            .fold(BddPlan::ConstTrue, |acc, (dist, sample)| {
                BddPlan::and(
                    acc,
                    BddPlan::iff(dist.clone(), BddPlan::from_bool(sample.clone())),
                )
            })
    }
    pub fn dists(&self) -> Vec<BddPlan> {
        self.out
            .iter()
            .cloned()
            .map(|v| match v {
                EVal::EBdd(b) => Ok(b),
                _ => errors::typecheck_failed(),
            })
            .collect::<Result<Vec<BddPlan>>>()
            .unwrap()
    }
}
impl Default for EOutput {
    fn default() -> Self {
        EOutput {
            out: Default::default(),
            accept: BddPlan::ConstTrue,
            samples: Default::default(),
            weightmap: Default::default(),
            substitutions: Default::default(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct SCtx {
    pub substitutions: HashMap<UniqueId, Vec<SVal>>,
    pub trace: Tr,
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
    pub trace: Tr,
}
impl SOutput {
    pub fn head(&self) -> SVal {
        self.out[0].clone()
    }
    pub fn pkg(&self) -> Output {
        Output::sample(self.clone())
    }
}
impl From<&SOutput> for SCtx {
    fn from(o: &SOutput) -> Self {
        SCtx {
            substitutions: o.substitutions.clone(),
            trace: o.trace.clone(),
        }
    }
}
/// whole-program context
#[derive(Debug, Clone, PartialEq)]
pub struct Ctx {
    pub exact: ECtx,
    pub sample: SCtx,
}
impl Default for Ctx {
    fn default() -> Self {
        Ctx {
            exact: Default::default(),
            sample: Default::default(),
        }
    }
}
impl Ctx {
    pub fn mk_eoutput(&self, exact: EOutput) -> Output {
        Output {
            exact,
            sample: self.sample.as_output(vec![]),
        }
    }
    pub fn mk_soutput(&self, sample: SOutput) -> Output {
        Output {
            exact: self.exact.as_output(vec![]),
            sample,
        }
    }
    pub fn new_from_eoutput(&self, exact: &EOutput) -> Self {
        let mut ctx = self.clone();
        ctx.exact = ECtx::from(exact);
        ctx
    }
    pub fn new_from_soutput(&self, sample: &SOutput) -> Self {
        let mut ctx = self.clone();
        ctx.sample = SCtx::from(sample);
        ctx
    }
}
impl From<&Output> for Ctx {
    fn from(o: &Output) -> Self {
        Ctx {
            exact: ECtx::from(&o.exact),
            sample: SCtx::from(&o.sample),
        }
    }
}

/// whole-program output, which can be one of: exact compilation, sampling
/// compilation, MLS semantics
#[derive(Debug, Clone, PartialEq)]
pub struct Output {
    pub exact: EOutput,
    pub sample: SOutput,
}
impl Default for Output {
    fn default() -> Self {
        Output {
            exact: Default::default(),
            sample: Default::default(),
        }
    }
}
impl Output {
    // pub fn new_from_eoutput(ctx: &Ctx, exact: &EOutput) -> Output {
    //     let mut ctx = ctx.clone();
    //     ctx.exact = ECtx::from(exact);
    //     ctx
    // }
    // pub fn new_from_soutput(&self, sample: &SOutput) -> Self {
    //     let mut ctx = self.clone();
    //     ctx.sample = SCtx::from(sample);
    //     ctx
    // }
    pub fn exact(exact: EOutput) -> Output {
        Output {
            exact,
            sample: Default::default(),
        }
    }
    pub fn sample(sample: SOutput) -> Output {
        Output {
            sample,
            exact: Default::default(),
        }
    }
    // pub fn for_sample_lang_sample(ctx: &Context, sample: bool, dist: BddPtr) -> Output {
    //     let newsample = if sample { dist } else { dist.not() };
    //     Output {
    //         out: vec![],
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

    // pub fn for_sample_lang(ctx: &Context) -> Output {
    //     // let accept = BddPtr::from_bool(sample.unwrap_or_else(|| false));
    //     Output {
    //         out: vec![BddPtr::PtrTrue],
    //         accept: ctx.accept,
    //         samples: ctx.samples,

    //         // pass through
    //         substitutions: ctx.substitutions.clone(),
    //         weightmap: ctx.weightmap.clone(),

    //         // unused
    //         probabilities: vec![Probability::new(1.0)],
    //         // importance: I::Weight(1.0),
    //         ssubstitutions: ctx.ssubstitutions.clone(),
    //         sout: vec![],
    //     }
    // }
    // pub fn sval(&self) -> Vec<SVal> {
    //     self.sout.clone()
    // }
    // pub fn sint(&self) -> u64 {
    //     SVal::vec_as(&self.sout, &SVal::as_int)
    // }
    // pub fn sbool(&self) -> bool {
    //     SVal::vec_as(&self.sout, &SVal::as_bool)
    // }
    // pub fn sfloat(&self) -> f64 {
    //     SVal::vec_as(&self.sout, &SVal::as_float)
    // }
    // pub fn sfloats(&self) -> Vec<f64> {
    //     self.sval().iter().map(|v| v.as_float()).collect()
    // }
}
