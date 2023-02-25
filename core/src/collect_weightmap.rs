/// colllect the weight map in advance of compilation.
/// TODO: if there is an observe inside a sample's subprogram, we need to remove
/// the entry after exiting the sample, or change the weight to the constant weight.
use crate::annotate::grammar::*;
use crate::compile::CompileError;
use crate::grammar::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use std::collections::HashMap;
