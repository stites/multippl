use crate::annotate::{InvMap, LabelEnv};
use crate::compile::eval::State;
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

pub fn run(code: &str) -> Result<(Output, PQ)> {
    let mgr = make_mgr_h(code)?;
    let (o, _, pq) = runner(code, mgr, &Options::stoch())?;
    Ok((o, pq))
}

pub fn run_with_seed(code: &str, seed: u64) -> Result<(Output, Option<StdRng>, PQ)> {
    let mgr = make_mgr_h(code)?;
    runner(code, mgr, &Options::seed(seed))
}

pub fn runner(code: &str, mut mgr: Mgr, opt: &Options) -> Result<(Output, Option<StdRng>, PQ)> {
    let p = crate::parser::program::parse(code)?;
    let p = crate::typeinf::typeinference(&p)?;
    let p = crate::typecheck::typecheck(&p)?;
    let p = crate::desugar::desugar(&p)?;
    let p = SymEnv::default().uniquify(&p)?.0;
    let ar = LabelEnv::new().annotate(&p)?;
    let p = ar.program;
    let maxlbl = ar.maxbdd.0;

    // let mut mgr = Mgr::new_default_order(mxlbl as usize);
    let mut rng = opt.rng();
    let sample_pruning = opt.opt;

    let mut state = State::new(&mut mgr, Some(&mut rng), sample_pruning);
    let (out, ptrace) = state.eval_program(&p)?;

    Ok((out, state.rng.cloned(), state.pq))
}

pub fn make_mgr_h(code: &str) -> Result<Mgr> {
    let p = crate::parser::program::parse(code)?;
    let p = crate::typeinf::typeinference(&p)?;
    let p = crate::typecheck::typecheck(&p)?;
    let p = crate::desugar::desugar(&p)?;
    let p = SymEnv::default().uniquify(&p)?.0;
    let ar = LabelEnv::new().annotate(&p)?;
    let maxlbl = ar.maxbdd.0;

    Ok(Mgr::new_default_order(maxlbl as usize))
}
