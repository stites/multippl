#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)] // for Gamma : )
// temporary as I convert to using types
#![allow(unused_variables)]
#![allow(unreachable_patterns)]
#![allow(clippy::all)]

// core types
pub mod data;
pub use data::*;

pub mod grammar;
pub use grammar::*;

// Entry points
pub mod parser;
pub mod typeinf;

// intermediate stages
mod annotate;
mod typecheck;
mod uniquify;

// interface
pub mod analysis;
pub mod compile;
pub mod inference;
pub mod pipeline;
pub use pipeline::*;
pub use typeinf::grammar::ProgramInferable;

// extras
pub mod grids;
mod utils;

#[cfg(test)]
mod tests;
