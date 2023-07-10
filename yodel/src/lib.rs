// #![allow(dead_code)]
#![allow(unused_imports)]
// #![allow(mixed_script_confusables)] // for Gamma : )
// // temporary as I convert to using types
#![allow(unused_variables)]
// #![allow(unreachable_patterns)]

// // core types
pub mod data;
pub use data::*;

pub mod grammar;
pub use grammar::*;

// // Entry points
pub mod parser;

pub mod typeinf;

mod typecheck;
mod uniquify;

// interface
pub mod analysis;
pub mod compile;
pub mod inference;
pub mod pipeline;
pub use crate::analysis::sampling::*;
pub use crate::typeinf::grammar::ProgramInferable;
pub use pipeline::*;

// extras
pub mod bayesian_network;
pub mod grids;
pub mod utils;

#[cfg(test)]
mod tests;
