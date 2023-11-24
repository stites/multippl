use crate::annotate::grammar::Var;
// use crate::data::context::Context;
// use crate::data::importance::{Importance, I};
use crate::data::errors;
use crate::data::{Result, Weight, WeightMap};
use crate::grammar::{EVal, SVal};
use crate::uniquify::grammar::UniqueId;
use crate::*;
use itertools::izip;
use num_traits::identities::Zero;
use rsdd::sample::probability::Probability;
use std::collections::HashMap;

pub type SubstMap<V> = HashMap<UniqueId, V>;
pub type Tr = Vec<(SVal, Dist, f64, Option<UniqueId>)>;

/// exact compilation context
#[derive(Debug, Clone, PartialEq)]
pub struct ECtx {
    pub accept: BddPtr,
    #[cfg(feature = "debug_samples")]
    pub samples: HashMap<BddPtr, bool>,
    #[cfg(not(feature = "debug_samples"))]
    pub samples: BddPtr,
    pub substitutions: SubstMap<EVal>,
    pub weightmap: WeightMap,
}
pub trait GetSamples {
    /// optionally prune these samples.
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr;

    fn samples(&self, mgr: &mut Mgr, prune: bool) -> BddPtr {
        if prune {
            BddPtr::PtrTrue
        } else {
            Self::compile_samples(self, mgr)
        }
    }
}

#[cfg(feature = "debug_samples")]
fn _all_samples(samples: &HashMap<BddPtr, bool>, mgr: &mut Mgr) -> BddPtr {
    samples.iter().fold(BddPtr::PtrTrue, |ss, (dist, s)| {
        let nxt = mgr.iff(dist.clone(), BddPtr::from_bool(*s));
        mgr.and(ss, nxt)
    })
}

impl GetSamples for ECtx {
    #[cfg(feature = "debug_samples")]
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr {
        _all_samples(&self.samples, mgr)
    }
    #[cfg(not(feature = "debug_samples"))]
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr {
        self.samples
    }
}

#[cfg(feature = "debug_samples")]
fn default_samples() -> HashMap<BddPtr, bool> {
    Default::default()
}
#[cfg(not(feature = "debug_samples"))]
fn default_samples() -> BddPtr {
    BddPtr::PtrTrue
}
impl GetSamples for Ctx {
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr {
        GetSamples::compile_samples(&self.exact, mgr)
    }
}

impl ECtx {
    pub fn default_with_data(dv: &DataView1) -> Self {
        ECtx {
            accept: BddPtr::PtrTrue,
            samples: default_samples(),
            substitutions: dv.exact.clone(),
            weightmap: Default::default(),
        }
    }
}
impl From<&EOutput> for ECtx {
    fn from(out: &EOutput) -> Self {
        ECtx {
            accept: out.accept,
            samples: out.samples,
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
    pub fn as_output(&self, out: Option<EVal>) -> EOutput {
        EOutput {
            out,
            accept: self.accept,
            samples: self.samples,
            substitutions: self.substitutions.clone(),
            weightmap: self.weightmap.clone(),
        }
    }
}

/// exact compilation output
#[derive(Debug, Clone, PartialEq)]
pub struct EOutput {
    /// compiled distributions
    pub out: Option<EVal>,
    /// acceptance criteria
    pub accept: BddPtr,
    /// sample consistency
    #[cfg(feature = "debug_samples")]
    pub samples: HashMap<BddPtr, bool>,
    #[cfg(not(feature = "debug_samples"))]
    pub samples: BddPtr,
    /// compiled weightmap
    pub weightmap: WeightMap,
    /// substitution environment
    pub substitutions: SubstMap<EVal>,
}
impl GetSamples for EOutput {
    #[cfg(feature = "debug_samples")]
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr {
        _all_samples(&self.samples, mgr)
    }
    #[cfg(not(feature = "debug_samples"))]
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr {
        self.samples
    }
}
pub fn as_dists(outs: Vec<EVal>) -> Vec<BddPtr> {
    outs.iter()
        .cloned()
        .filter(|v| match v {
            EVal::EBdd(b) => true,
            a => false,
        })
        .map(|v| match v {
            EVal::EBdd(b) => Ok(b),
            a => errors::typecheck_failed(&format!("eoutput projection into bdds got {a:?}")),
        })
        .collect::<Result<Vec<_>>>()
        .unwrap()
}
impl EOutput {
    pub fn from_anf_out(ctx: &ECtx, out: Option<EVal>) -> Self {
        EOutput {
            out,
            accept: ctx.accept,
            samples: ctx.samples,
            substitutions: ctx.substitutions.clone(),
            weightmap: ctx.weightmap.clone(),
        }
    }
    pub fn pkg(&self) -> Output {
        Output::exact(self.clone())
    }
    #[cfg(feature = "debug_samples")]
    pub fn samples(&self, mgr: &mut Mgr) -> BddPtr {
        self.samples
            .iter()
            .fold(BddPtr::PtrTrue, |acc, (dist, sample)| {
                let s = mgr.iff(dist.clone(), BddPtr::from_bool(sample.clone()));
                mgr.and(acc, s)
            })
    }
    #[cfg(not(feature = "debug_samples"))]
    pub fn samples(&self, mgr: &mut Mgr) -> BddPtr {
        self.samples
    }

    pub fn dists(&self) -> Vec<BddPtr> {
        match &self.out {
            Some(EVal::EBdd(b)) => vec![*b],
            Some(EVal::EProd(bs)) => as_dists(bs.to_vec()),
            a => errors::typecheck_failed(&format!("eoutput projection into bdds got {a:?}"))
                .unwrap(),
        }
    }
}
impl Default for EOutput {
    fn default() -> Self {
        EOutput {
            out: Default::default(),
            accept: BddPtr::PtrTrue,
            samples: default_samples(),
            weightmap: Default::default(),
            substitutions: Default::default(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct SCtx {
    pub substitutions: SubstMap<SVal>,
    pub trace: Tr,
}
impl SCtx {
    pub fn as_output(&self, out: Option<SVal>) -> SOutput {
        SOutput {
            out,
            trace: self.trace.clone(),
            substitutions: self.substitutions.clone(),
        }
    }
    pub fn default_with_data(dv: &DataView1) -> Self {
        SCtx {
            substitutions: dv.sampling.clone(),
            trace: Default::default(),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct SOutput {
    /// compiled output
    pub out: Option<SVal>,
    /// sampling substitutions
    pub substitutions: SubstMap<SVal>,
    // sampled values traces (and ids)
    pub trace: Tr,
}
impl SOutput {
    pub fn head(&self) -> SVal {
        self.out.clone().unwrap()
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

impl Ctx {
    pub fn default_with_data(dv: &DataView1) -> Self {
        Ctx {
            exact: ECtx::default_with_data(dv),
            sample: SCtx::default_with_data(dv),
        }
    }
}
impl Ctx {
    pub fn mk_eoutput(&self, exact: EOutput) -> Output {
        Output {
            exact,
            sample: self.sample.as_output(None),
        }
    }
    pub fn mk_soutput(&self, sample: SOutput) -> Output {
        Output {
            exact: self.exact.as_output(None),
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
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Output {
    pub exact: EOutput,
    pub sample: SOutput,
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
