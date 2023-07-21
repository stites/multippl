use crate::annotate::grammar::SampledVar;
// pub use crate::data::context::Context;
// pub use crate::data::errors::{CompileError, Result};
pub use crate::data::output::Output;
// pub use crate::data::{Weight, WeightMap};
// use crate::grammar::*;
// use crate::uniquify::grammar::UniqueId;
// use crate::utils::render::*;
use crate::*;
// use CompileError::*;

// use std::fmt::*;

::ttg::phase!(pub struct Trace: Box<Output> {
    // except for distributions! we need to keep
    // these around to add the info into the trace
    ADistExt<SVal>: (SampledVar, SOutput),
});

::ttg::alias!(Trace as Tr + (Program, EExpr, SExpr, Anf<Var>));
