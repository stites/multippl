use crate::annotate::{InvMap, LabelEnv};
use crate::compile::eval::State;
use crate::compile::grammar::ProgramTr;
use crate::compile::grammar::Trace;
use crate::data::*;
use crate::typecheck::{
    grammar::{EExprTyped, ProgramTyped},
    typecheck,
};
use crate::typeinf::grammar::ProgramInferable;
use crate::typeinf::typeinference;
use crate::uniquify::SymEnv;
use rand::rngs::StdRng;

#[derive(Default, Debug, Clone)]
pub struct Options {
    // pub seed: Option<u64>,
    pub seed: Option<StdRng>,
    pub debug: bool, // overrides seed
    pub opt: bool,   // use optimizations
    pub stats_window: u64,
}
impl Options {
    pub fn rng(&self) -> StdRng {
        match &self.seed {
            None => rand::SeedableRng::from_entropy(),
            Some(s) => s.clone(),
        }
    }
    pub fn stoch() -> Self {
        Default::default()
    }
    pub fn seed(s: u64) -> Self {
        Self {
            seed: Some(rand::SeedableRng::seed_from_u64(s)),
            ..Default::default()
        }
    }
    pub fn debug() -> Self {
        Self {
            debug: true,
            opt: false,
            ..Default::default()
        }
    }
    pub fn new(
        seed: Option<u64>,
        debug: bool,       // overrides seed
        opt: bool,         // use optimizations
        stats_window: u64, // use optimizations
    ) -> Self {
        Self {
            seed: seed.map(|s| rand::SeedableRng::seed_from_u64(s)),
            debug,
            opt,
            stats_window,
        }
    }
}

pub struct ROut {
    pub out: Output,
    pub rng: Option<StdRng>,
    pub mgr: Mgr,
    pub pq: PQ,
    pub prg: ProgramTr,
}

pub struct PartialROut {
    pub out: Output,
    pub pq: PQ,
    pub rng: Option<StdRng>,
    pub prg: ProgramTr,
}
impl PartialROut {
    fn to_rout(&self, mgr: Mgr) -> ROut {
        ROut {
            mgr,
            out: self.out.clone(),
            rng: self.rng.clone(),
            pq: self.pq,
            prg: self.prg.clone(),
        }
    }
}
pub fn run(code: &str) -> Result<ROut> {
    let mut mgr = make_mgr(code)?;
    let opt = Options::stoch();
    let r = runner(code, &mut mgr, &mut opt.rng(), &opt)?;
    Ok(r.to_rout(mgr))
}

pub fn run_with(code: &str, opt: &Options) -> Result<ROut> {
    let mut mgr = make_mgr(code)?;
    let r = runner(code, &mut mgr, &mut opt.rng(), opt)?;
    Ok(r.to_rout(mgr))
}

pub fn runner(code: &str, mgr: &mut Mgr, rng: &mut StdRng, opt: &Options) -> Result<PartialROut> {
    let p = crate::parser::program::parse(code)?;
    let p = crate::typeinf::typeinference(&p)?;
    let p = crate::typecheck::typecheck(&p)?;
    let p = crate::desugar::desugar(&p)?;
    let p = SymEnv::default().uniquify(&p)?.0;
    let ar = LabelEnv::new().annotate(&p)?;
    let p = ar.program;
    let maxlbl = ar.maxbdd.0;

    let sample_pruning = opt.opt;

    let mut state = State::new(mgr, Some(rng), sample_pruning);
    let (out, ptrace) = state.eval_program(&p)?;

    Ok(PartialROut {
        out,
        pq: state.pq,
        rng: state.rng.cloned(),
        prg: ptrace,
    })
}

pub fn make_mgr(code: &str) -> Result<Mgr> {
    let p = crate::parser::program::parse(code)?;
    let p = crate::typeinf::typeinference(&p)?;
    let p = crate::typecheck::typecheck(&p)?;
    let p = crate::desugar::desugar(&p)?;
    let p = SymEnv::default().uniquify(&p)?.0;
    let ar = LabelEnv::new().annotate(&p)?;
    let maxlbl = ar.maxbdd.0;

    Ok(Mgr::new_default_order(maxlbl as usize))
}
