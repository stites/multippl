pub mod anf;
// pub mod eval;
pub mod grammar;

use crate::annotate::grammar::ProgramAnn;
// use crate::compile::eval::State;
use crate::compile::grammar::*;
use crate::data::*;
use crate::grammar::Program;

// pub fn debug(s: &mut State, p: &ProgramAnn) -> Result<(Compiled, ProgramTr)> {
//     s.eval_program(p)
// }

// pub fn compile(s: &mut State, p: &ProgramAnn) -> Result<Compiled> {
//     Ok(debug(s, p)?.0)
// }
