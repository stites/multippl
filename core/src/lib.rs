#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)] // for Gamma : )
// temporary as I convert to using types
#![allow(unused_variables)]
#![allow(clippy::all)]

// core types
pub use rsdd::sample::probability::Probability;
pub mod data;
pub mod grammar;
pub mod grammar_macros;

// compiler
mod annotate;
pub mod parser; // <<< user entrypoint
mod typecheck;
pub mod typeinf; // <<< dev entrypoint
mod uniquify;

// interface
pub mod analysis;
pub mod compile;
pub mod inference;
pub mod pipeline;

// extras
pub mod grids;
mod utils;

#[cfg(test)]
mod tests;

pub use pipeline::*;
pub use typeinf::grammar::ProgramInferable;
