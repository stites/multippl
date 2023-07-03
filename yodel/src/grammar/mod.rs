/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
pub mod anf;
pub mod classes;
pub mod exact;
pub mod function;
pub mod program;
pub mod sampling;
pub mod ttg;
pub mod undecorated;

pub use crate::grammar::exact::*;
pub use crate::grammar::sampling::*;

pub use crate::grammar::anf::*;
pub use crate::grammar::classes::*;
pub use crate::grammar::function::*;
pub use crate::grammar::program::*;
pub use crate::grammar::ttg::*;
pub use crate::grammar::undecorated::*;

pub mod macros;
pub use crate::grammar::macros::*;
