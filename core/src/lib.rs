#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)] // for Gamma : )
// temporary as I convert to using types
#![allow(unused_variables)]
#![allow(clippy::clone_on_copy)]
#![allow(clippy::type_complexity)]
#![allow(clippy::redundant_clone)]
#![allow(clippy::useless_format)]
#![allow(clippy::needless_return)]
#![allow(clippy::upper_case_acronyms)]
#![allow(clippy::single_component_path_imports)]
#![allow(clippy::enum_variant_names)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::let_and_return)]
#![allow(clippy::len_zero)]
#![allow(clippy::assign_op_pattern)]
#![allow(clippy::unnecessary_cast)]
#![allow(clippy::unnecessary_lazy_evaluations)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::ptr_arg)]
// use itertools::*;
// use rsdd::builder::bdd_builder::BddManager;
// use rsdd::builder::cache::all_app::AllTable;
// use std::collections::HashMap;
// use std::string::String;
// use tracing::debug;

mod grammar;
mod grammar_macros;

mod annotate;
mod compile;
mod inference;
mod parser;
mod render;
#[cfg(test)]
mod tests;
mod typecheck;

use crate::annotate::annotate;
use crate::compile::{compile, CompileError, Compiled, Env};
use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
use crate::typecheck::typecheck;

pub fn run(env: &mut Env, p: &ProgramTyped) -> Result<Compiled, CompileError> {
    compile(env, &annotate(&typecheck(p)?)?)
}

#[cfg(test)]
mod active_tests {
    use super::*;
    use crate::compile::*;
    use crate::grammar::*;
    use crate::grammar_macros::*;
    use inference::*;
    use tests::*;
    use tracing_test::traced_test;
    #[test]
    #[ignore = "this test will always break until we are doing data flow analysis"]
    // #[traced_test]
    fn free_variables_0() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
               "x" ; B!() ;= flip!(1/3);
               "y" ; B!() ;= sample!(var!("x"));
               ...? ret ; B!()
            ])
        };
        let n = 10000;

        check_approx1("free/x ", 1.0 / 3.0, &mk(var!("x")), n);
        // FIXME: still broken! getting 1/2 instead of 1/3
        check_approx1("free/y ", 1.0 / 3.0, &mk(var!("y")), n);

        check_invariant("free/y ", None, None, &mk(var!("y")));
        check_invariant("free/y ", None, None, &mk(var!("x")));
    }

    #[test]
    #[ignore = "This will be broken until we reintroduce code at FIXME(#1)."]
    // #[traced_test]
    fn free_variables_1() {
        let problem = {
            Program::Body(lets![
               "x" ; B!() ;= flip!(1/3);
               "l" ; B!() ;= sample!(var!("x"));
               "_" ; B!() ;= observe!(var!("x"));
               ...? var!("l") ; B!()
            ])
        };
        check_approx1("free/!!", 1.0 / 3.0, &problem, 10000);
    }
    #[test]
    // #[traced_test]
    #[ignore = "punt till data flow analysis"]
    fn free_variables_2() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "x" ; b!() ;= flip!(1/3);
                "l" ; b!() ;= sample!(
                    lets![
                        "x0" ; b!() ;= flip!(1/5);
                        ...? b!("x0" || "x") ; b!()
                    ]);
               "_" ; b!() ;= observe!(var!("x")); // is this a problem?
               ...? ret ; b!()
            ])
        };
        check_approx1("free/?1", 1.0 / 1.0, &mk(var!("x")), 1000);
        check_approx1("free/?2", 1.0 / 1.0, &mk(var!("l")), 1000); // FIXME: need to derive the right number for this one
    }

    // free variable edge case
    #[test]
    // #[traced_test]
    #[ignore = "I suspect this is also funky (but gives a seemingly obscure result) because of free_variables_0 issue"]
    fn free_variables_shared() {
        let p = Program::Body(lets![
           "x" ; b!() ;= flip!(1/3);
           "l" ; b!() ;= sample!(var!("x"));
           "r" ; b!() ;= sample!(var!("x"));
           ...? b!("l" && "r") ; b!()
        ]);
        check_approx1("shared ", 1.0 / 3.0, &p, 1000);
    }

    #[test]
    #[ignore = "also broken due to same reason as free_variables_0"]
    // #[traced_test]
    fn free_variables_shared_tuple() {
        let p = Program::Body(lets![
           "x" ; b!()     ;= flip!(1/3);
           "z" ; b!(B, B) ;= sample!(b!("x", "x"));
           ...? b!("z" ; b!(B, B)); b!(B, B)
        ]);
        check_approx("sharedtuple", vec![1.0 / 3.0, 1.0 / 3.0], &p, 1000);
    }

    /// represents another semantic bug, still need to grapple how this all works, though
    #[test]
    #[ignore = "I'm confident that this is a data flow analysis problem (in that sample formulas need to be added to the accepting criteria)"]
    // #[traced_test]
    fn ite_3_with_one_sample_hard1() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "x" ; b!() ;= flip!(2/3);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                    then { sample!(flip!(3/4)) }
                    else { flip!(1/5) });
                "_" ; b!() ;= observe!(b!("x" || "y"));
                ...? ret ; b!()
            ])
        };
        let n = 1000;
        check_approx1("ite_3/y-sample3/4 ", 0.772727273, &mk(b!("y")), n); // dice's answer for 3/4 @ sample site
                                                                           // check_approx1("ite_3/y  ", 0.545454545, &mk(b!("y")), n); // dice's answer for 2/4 @ sample site
    }
    #[test]
    #[ignore = "I'm confident that this is a data flow analysis problem (in that sample formulas need to be added to the accepting criteria)"]
    fn ite_3_with_one_sample_hard2() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "x" ; b!() ;= flip!(1/3);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                    then { sample!(flip!(1/4)) }
                    else { flip!(1/5) });
                "_" ; b!() ;= observe!(b!("x" || "y"));
                ...? ret ; b!()
            ])
        };
        let n = 1000;
        check_approx1("ite_3/x&y", 0.178571429, &mk(b!("x" && "y")), n * n * n);
    }
}
