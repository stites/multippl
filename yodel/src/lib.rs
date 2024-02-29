#![allow(dead_code)]
#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]
#![allow(unused_imports)]
// // temporary as I convert to using types
#![allow(unused_variables)]
#![allow(clippy::redundant_closure_call)] // for some tests
                                          // #![allow(unreachable_patterns)]

// core types
pub mod data;
pub use data::*;

pub mod grammar;
pub use grammar::*;

// Entry points
pub mod parser;

pub mod typeinf;
pub use crate::typeinf::grammar::ProgramInferable;

// intermediate stages

mod typecheck;

mod desugar;

mod uniquify;

mod annotate;

// pub mod analysis; // punt on this
// pub use crate::analysis::sampling::*;

// interface
pub mod compile;

pub mod pipeline;
pub use pipeline::*;

pub mod inference;

// extras
pub mod bayesian_network;
pub mod upcast;
// pub mod grids;
pub mod utils;

#[cfg(test)]
mod tests;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
