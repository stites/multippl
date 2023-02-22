#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)] // for Gamma : )
// temporary as I convert to using types
#![allow(unused_variables)]
#![allow(clippy::clone_on_copy)]
#![allow(clippy::type_complexity)]
#![allow(clippy::redundant_clone)]
#![allow(clippy::useless_format)]
#![allow(clippy::needless_return)]
#![allow(clippy::upper_case_acronyms)]
#![allow(clippy::single_component_path_imports)]
#![allow(clippy::enum_variant_names)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::let_and_return)]
#![allow(clippy::len_zero)]
#![allow(clippy::assign_op_pattern)]
#![allow(clippy::unnecessary_cast)]
#![allow(clippy::unnecessary_lazy_evaluations)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::ptr_arg)]
// use itertools::*;
// use rsdd::builder::bdd_builder::BddManager;
// use rsdd::builder::cache::all_app::AllTable;
// use std::collections::HashMap;
// use std::string::String;
// use tracing::debug;

mod grammar;
mod grammar_macros;

mod annotate;
mod compile;
mod inference;
mod parser;
mod render;
#[cfg(test)]
mod tests;
mod typecheck;

use crate::annotate::annotate;
use crate::compile::{compile, CompileError, Compiled, Env};
use crate::typecheck::{grammar::ProgramTyped, typecheck};

pub fn run(env: &mut Env, p: &ProgramTyped) -> Result<Compiled, CompileError> {
    compile(env, &annotate(&typecheck(p)?)?)
}

#[cfg(test)]
mod active_tests {
    use super::*;
    use crate::compile::*;
    use grammar::*;
    use inference::*;
    use tests::*;
    use tracing_test::traced_test;
}
