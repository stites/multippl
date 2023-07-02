/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
mod anf;
mod classes;
mod exact;
mod function;
mod program;
mod sampling;
mod ttg;
mod undecorated;

// mod macros;

pub use crate::grammar::exact::sugar::*;
pub use crate::grammar::exact::*;
pub use crate::grammar::sampling::*;

pub use crate::grammar::anf::*;
pub use crate::grammar::classes::*;
pub use crate::grammar::function::*;
pub use crate::grammar::program::*;
pub use crate::grammar::ttg::*;
pub use crate::grammar::undecorated::*;
// pub use crate::grammar::macros::*;
