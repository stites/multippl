use crate::annotate::grammar::Var;
// use crate::data::context::Context;
// use crate::data::importance::{Importance, I};
use crate::data::errors;
use crate::data::HashMap;
use crate::data::{Result, Weight};
use crate::grammar::{EVal, SVal};
use crate::uniquify::grammar::UniqueId;
use crate::*;
use itertools::izip;
use num_traits::identities::Zero;
use rsdd::sample::probability::Probability;

pub type SubstMap<V> = HashMap<UniqueId, V>;
pub type Tr = Vec<(SVal, Dist, f64, Option<UniqueId>)>;

/// exact compilation context
#[derive(Debug, Clone, PartialEq)]
pub struct ECtx {
    pub accept: BddPtr,
    pub samples: BddPtr,
    pub substitutions: SubstMap<EVal>,
}
pub trait GetSamples {
    /// optionally prune these samples.
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr;

    #[inline(always)]
    fn samples(&self, mgr: &mut Mgr, prune: bool) -> BddPtr {
        if prune {
            BddPtr::PtrTrue
        } else {
            Self::compile_samples(self, mgr)
        }
    }
}

impl GetSamples for ECtx {
    #[inline(always)]
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr {
        self.samples
    }
}

#[inline(always)]
fn default_samples() -> BddPtr {
    BddPtr::PtrTrue
}
impl GetSamples for Ctx {
    #[inline(always)]
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr {
        GetSamples::compile_samples(&self.exact, mgr)
    }
}

impl ECtx {
    #[inline(always)]
    pub fn default_with_data(dv: &DataView1) -> Self {
        ECtx {
            accept: BddPtr::PtrTrue,
            samples: default_samples(),
            substitutions: dv.exact.clone(),
        }
    }
}
impl From<&EOutput> for ECtx {
    #[inline(always)]
    fn from(out: &EOutput) -> Self {
        ECtx {
            accept: out.accept,
            samples: out.samples,
            substitutions: out.substitutions.clone(),
        }
    }
}
impl ECtx {
    #[inline(always)]
    pub fn as_output(&self, out: Option<EVal>) -> EOutput {
        EOutput {
            out,
            accept: self.accept,
            samples: self.samples,
            substitutions: self.substitutions.clone(),
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
    pub samples: BddPtr,
    /// substitution environment
    pub substitutions: SubstMap<EVal>,
}
impl GetSamples for EOutput {
    #[inline(always)]
    fn compile_samples(&self, mgr: &mut Mgr) -> BddPtr {
        self.samples
    }
}
#[inline(always)]
pub fn as_dists(outs: Vec<EVal>) -> Vec<BddPtr> {
    outs.iter()
        // .cloned()
        .filter(|v| match *v {
            EVal::EBdd(b) => true,
            a => false,
        })
        .cloned()
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
            substitutions: Default::default(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct SCtx {
    pub substitutions: SubstMap<SVal>,
}
impl SCtx {
    pub fn as_output(&self, out: Option<SVal>) -> SOutput {
        SOutput {
            out,
            substitutions: self.substitutions.clone(),
        }
    }
    pub fn default_with_data(dv: &DataView1) -> Self {
        SCtx {
            substitutions: dv.sampling.clone(),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct SOutput {
    /// compiled output
    pub out: Option<SVal>,
    /// sampling substitutions
    pub substitutions: SubstMap<SVal>,
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
    #[inline(always)]
    pub fn default_with_data(dv: &DataView1) -> Self {
        Ctx {
            exact: ECtx::default_with_data(dv),
            sample: SCtx::default_with_data(dv),
        }
    }
}
impl Ctx {
    #[inline(always)]
    pub fn mk_eoutput(&self, exact: EOutput) -> Output {
        Output {
            exact,
            sample: self.sample.as_output(None),
        }
    }
    #[inline(always)]
    pub fn mk_soutput(&self, sample: SOutput) -> Output {
        Output {
            exact: self.exact.as_output(None),
            sample,
        }
    }
    #[inline(always)]
    pub fn new_from_eoutput(&self, exact: &EOutput) -> Self {
        let mut ctx = self.clone();
        ctx.exact = ECtx::from(exact);
        ctx
    }
    #[inline(always)]
    pub fn new_from_soutput(&self, sample: &SOutput) -> Self {
        let mut ctx = self.clone();
        ctx.sample = SCtx::from(sample);
        ctx
    }
}
impl From<&Output> for Ctx {
    #[inline(always)]
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
    #[inline(always)]
    pub fn exact(exact: EOutput) -> Output {
        Output {
            exact,
            sample: Default::default(),
        }
    }
    #[inline(always)]
    pub fn sample(sample: SOutput) -> Output {
        Output {
            sample,
            exact: Default::default(),
        }
    }
}
