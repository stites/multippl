use super::anf::Anf;
use super::exact::*;
use super::program::Program;
use super::sampling::*;
use super::ttg::*;

// UD comprises of the default, undecorated grammar.
// all extensions are of type ().
ttg::phase!(pub struct UD);
ttg::alias!(UD + (Program, EExpr, SExpr, Anf<Var>));
