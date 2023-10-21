use crate::annotate::{InvMap, LabelEnv};
use crate::compile::eval::State;
use crate::compile::grammar::ProgramTr;
use crate::compile::grammar::Trace;
use crate::typecheck::{
    grammar::{EExprTyped, ProgramTyped},
    typecheck,
};
use crate::typeinf::grammar::ProgramInferable;
use crate::typeinf::typeinference;
use crate::uniquify::SymEnv;
use crate::*;
use rand::rngs::StdRng;
use tracing::debug;

#[derive(Debug, Clone)]
pub struct Options {
    // pub seed: Option<u64>,
    pub seed: Option<StdRng>,
    pub exact_only: bool,  // strip all sample statements from a program.
    pub debug: bool,       // overrides seed
    pub opt: bool,         // use optimizations
    pub stats_window: u64, // remove this?
}
impl Default for Options {
    fn default() -> Self {
        Options {
            seed: None,
            exact_only: false,
            debug: false,
            opt: false,
            stats_window: 0,
        }
    }
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
        exact_only: bool,
        debug: bool,       // overrides seed
        opt: bool,         // use optimizations
        stats_window: u64, // use optimizations
    ) -> Self {
        Self {
            seed: seed.map(|s| rand::SeedableRng::seed_from_u64(s)),
            debug,
            opt,
            stats_window,
            exact_only,
        }
    }
}

pub struct ROut {
    pub out: Output,
    pub rng: Option<StdRng>,
    pub mgr: Mgr,
    pub pq: LPQ,
}

pub struct PartialROut {
    pub out: Output,
    pub pq: LPQ,
    pub rng: Option<StdRng>,
}
impl PartialROut {
    pub fn to_rout(&self, mgr: Mgr) -> ROut {
        ROut {
            mgr,
            out: self.out.clone(),
            rng: self.rng.clone(),
            pq: self.pq,
        }
    }
}
#[macro_export]
macro_rules! run {
    ($code:expr) => {{
        let opt = $crate::pipeline::Options::stoch();
        run!($code, opt)
    }};
    ($code:expr; --split exact) => {{
        let mut opt = $crate::pipeline::Options::stoch();
        opt.exact_only = true;
        run!($code, opt)
    }};
    ($code:expr, $opt:expr) => {{
        match $crate::pipeline::run($code, &$opt) {
            Ok(o) => o,
            Err(e) => panic!(
                "\nCompiler Error!!!\n==============\n{}\n==============\n",
                e
            ),
        }
    }};
}

pub fn run(code: &str, opt: &Options) -> Result<ROut> {
    let (mut mgr, p, lenv) = make_mgr_and_ir(code)?;
    tracing::debug!(",====================================.");
    tracing::debug!("| manager compiled! building program |");
    tracing::debug!("`===================================='");
    let r = runner(&mut mgr, &mut opt.rng(), &opt, &p, &lenv)?;
    Ok(r.to_rout(mgr))
}

pub fn runner(
    mgr: &mut Mgr,
    rng: &mut StdRng,
    opt: &Options,
    p: &Program<crate::annotate::grammar::Annotated>,
    lenv: &LabelEnv,
) -> Result<PartialROut> {
    let sample_pruning = opt.opt;

    let mut state = State::new(mgr, Some(rng), sample_pruning, &lenv.funs);
    let out = state.eval_program(&p)?;
    tracing::debug!("program... compiled!");

    Ok(PartialROut {
        out,
        pq: state.log_pq(),
        rng: state.rng.cloned(),
    })
}

pub fn make_mgr(code: &str) -> Result<Mgr> {
    tracing::trace!("making manager");
    let p = crate::parser::program::parse(code)?;
    tracing::debug!("(parsed)");
    tracing::debug!("(parsed) >>> {p:?}");
    tracing::debug!("(parsed)");
    let p = crate::typeinf::typeinference(&p)?;
    tracing::debug!("(inferred)");
    tracing::debug!("(inferred) >>> {p:?}");
    tracing::debug!("(inferred)");
    let p = crate::typecheck::typecheck(&p)?;
    tracing::debug!("(checked)");
    tracing::debug!("(checked) >>> {p:?}");
    tracing::debug!("(checked)");
    let p = crate::desugar::desugar(&p)?;
    tracing::debug!("(desugared)");
    tracing::debug!("(desugared) >>> {p:?}");
    tracing::debug!("(desugared)");
    let mut env = SymEnv::default();
    let p = env.uniquify(&p)?.0;
    tracing::debug!("(uniquifyed)");
    tracing::debug!("(uniquifyed) >>> {p:?}");
    tracing::debug!("(uniquifyed)");
    let ar = LabelEnv::new(env.functions, env.fun_stats).annotate(&p)?;
    let p = ar.program;
    tracing::debug!("(annotated)");
    tracing::debug!("(annotated) >>> {p:?}");
    tracing::debug!("(annotated)");
    let maxlbl = ar.maxbdd.0;
    tracing::trace!("(manager created with max label: {maxlbl})");
    Ok(Mgr::new_default_order(maxlbl as usize))
}

pub fn make_mgr_and_ir(
    code: &str,
) -> Result<(Mgr, Program<crate::annotate::grammar::Annotated>, LabelEnv)> {
    tracing::info!("compiling code:\n{code}");
    tracing::trace!("making manager");
    let p = crate::parser::program::parse(code)?;
    tracing::debug!("(parsed)");
    tracing::debug!("(parsed) >>> {p:?}");
    tracing::debug!("(parsed)");
    let p = crate::typeinf::typeinference(&p)?;
    tracing::debug!("(inferred)");
    tracing::debug!("(inferred) >>> {p:?}");
    tracing::debug!("(inferred)");
    let p = crate::typecheck::typecheck(&p)?;
    tracing::debug!("(checked)");
    tracing::debug!("(checked) >>> {p:?}");
    tracing::debug!("(checked)");
    let p = crate::desugar::desugar(&p)?;
    tracing::debug!("(desugared)");
    tracing::debug!("(desugared) >>> {p:?}");
    tracing::debug!("(desugared)");
    let mut env = SymEnv::default();
    let p = env.uniquify(&p)?.0;
    tracing::debug!("(uniquifyed)");
    tracing::debug!("(uniquifyed) >>> {p:?}");
    tracing::debug!("(uniquifyed)");
    let mut lenv = LabelEnv::new(env.functions, env.fun_stats);
    let ar = lenv.annotate(&p)?;
    let p = ar.program;
    tracing::debug!("(annotated)");
    tracing::debug!("(annotated) >>> {p:?}");
    tracing::debug!("(annotated)");
    let maxlbl = ar.maxbdd.0;
    tracing::trace!("(manager created with max label: {maxlbl})");
    Ok((Mgr::new_default_order(maxlbl as usize), p, lenv))
}
