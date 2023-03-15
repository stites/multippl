#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)] // for Gamma : )
// temporary as I convert to using types
#![allow(unused_variables)]
#![allow(clippy::all)]

pub use rsdd::sample::probability::Probability;
pub mod compile;
pub mod grammar;
pub mod grammar_macros;
pub mod grids;
pub mod inference;
pub mod typecheck;

mod analysis;
mod interactions;
// mod analysis2;
mod annotate;
mod parser;
mod render;
mod uniquify;
mod utils;

#[cfg(test)]
mod tests;

#[derive(Default, Debug)]
pub struct Options {
    pub seed: Option<u64>,
    pub debug: bool, // overrides seed
    pub opt: bool,   // use optimizations
}
impl Options {
    pub fn rng(&self) -> StdRng {
        match self.seed {
            None => rand::SeedableRng::from_entropy(),
            Some(s) => rand::SeedableRng::seed_from_u64(s),
        }
    }
    pub fn stoch() -> Self {
        Default::default()
    }
    pub fn seed(s: u64) -> Self {
        Self {
            seed: Some(s),
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
        debug: bool, // overrides seed
        opt: bool,   // use optimizations
    ) -> Self {
        Self { seed, debug, opt }
    }
}

use crate::analysis::{Analysis, AnalysisEnv};
use crate::annotate::{InvMap, LabelEnv};
use crate::compile::{compile, CompileError, Compiled, Env, Mgr, Output, Result};
use crate::typecheck::{
    grammar::{ExprTyped, ProgramTyped},
    typecheck,
};
use crate::uniquify::SymEnv;
use rand::rngs::StdRng;

pub fn run(p: &ProgramTyped) -> Result<(Mgr, Output)> {
    let (m, c) = runner(p, &Default::default())?;
    Ok((m, c.as_output().unwrap()))
}
pub fn run_h(p: &ProgramTyped, mgr: &mut Mgr) -> Result<Output> {
    let c = runner_h(p, mgr, &Default::default())?;
    Ok(c.0.as_output().unwrap())
}

pub fn runner(p: &ProgramTyped, opt: &Options) -> Result<(Mgr, Compiled)> {
    let mut mgr = make_mgr(p);
    let (c, _, _) = runner_h(p, &mut mgr, opt)?;
    Ok((mgr, c))
}

pub fn make_mgr_h(p: &ProgramTyped) -> Result<Mgr> {
    let p = typecheck(p)?;
    let mut senv = SymEnv::default();
    let p = senv.uniquify(&p)?;
    let mut lenv = LabelEnv::new();
    let (p, vo, varmap, inv, mxlbl) = lenv.annotate(&p)?;

    Ok(Mgr::new_default_order(mxlbl as usize))
}
pub fn make_mgr(p: &ProgramTyped) -> Mgr {
    match make_mgr_h(p) {
        Ok(m) => m,
        Err(e) => panic!("{}", e),
    }
}

pub fn runner_h(
    p: &ProgramTyped,
    mgr: &mut Mgr,
    opt: &Options,
) -> Result<(Compiled, InvMap, Analysis)> {
    let p = typecheck(p)?;
    let mut senv = SymEnv::default();
    let p = senv.uniquify(&p)?;
    let mut lenv = LabelEnv::new();
    let (p, vo, varmap, inv, mxlbl) = lenv.annotate(&p)?;

    let mut aenv = AnalysisEnv::new(&varmap);
    let (p, ab) = aenv.decorate(&p, opt.opt)?;
    let ig = aenv.interaction_graph()?;

    let mut rng = opt.rng();
    let orng = if opt.debug { None } else { Some(&mut rng) };
    let mut env = Env::new(mgr, orng, opt.opt, inv.clone(), ab.clone()); // technically don't need this if I use the decorated vars in a clever way

    env.varmap = Some(varmap);

    let c = compile(&mut env, &p)?;
    tracing::debug!("hurray!");
    Ok((c, inv, ab))
}

#[cfg(test)]
mod active_tests {
    // use super::*;
    // use crate::annotate::LabelEnv;
    // use crate::compile::*;
    // use crate::compile::{compile, CompileError, Compiled, Env, Output, Result};
    // use crate::grammar::*;
    // use crate::grammar_macros::*;
    // use crate::typecheck::{grammar::ExprTyped, typecheck};
    // use crate::uniquify::SymEnv;
    // use inference::*;
    // use rsdd::builder::bdd_builder::*;
    // use rsdd::builder::cache::all_app::*;

    // use rsdd::repr::bdd::*;
    // use rsdd::repr::ddnnf::DDNNFPtr;
    // use rsdd::repr::var_label::*;
    // use rsdd::*;
    // use tests::*;
    // use tracing::*;
    // use tracing_test::traced_test;
}
