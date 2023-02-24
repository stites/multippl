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
mod collect_weightmap;
mod compile;
mod inference;
mod parser;
mod render;
#[cfg(test)]
mod tests;
mod typecheck;
mod uniquify;

use crate::annotate::LabelEnv;
use crate::collect_weightmap::collect_weightmap;
use crate::compile::{compile, CompileError, Compiled, Env};
use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
use crate::typecheck::typecheck;
use crate::uniquify::SymEnv;

pub fn run(env: &mut Env, p: &ProgramTyped) -> Result<Compiled, CompileError> {
    let p = typecheck(p)?;
    let mut senv = SymEnv::default();
    let p = senv.uniquify(&p)?;
    let mut lenv = LabelEnv::new();
    let (p, _, vo, inv) = lenv.annotate(&p)?;
    env.names = senv.names; // just for debugging, really.
    let wm = collect_weightmap(&p)?;
    env.weightmap = Some(wm);
    env.order = Some(vo);
    env.inv = Some(inv);
    let c = compile(env, &p);
    tracing::debug!("hurray!");
    c
}

#[cfg(test)]
mod active_tests {
    use super::*;
    use crate::compile::*;
    use crate::grammar::*;
    use crate::grammar_macros::*;
    use inference::*;
    use rsdd::builder::bdd_builder::*;
    use rsdd::builder::cache::all_app::*;

    use rsdd::repr::bdd::*;
    use rsdd::repr::ddnnf::DDNNFPtr;
    use rsdd::repr::var_label::*;
    use rsdd::*;
    use tests::*;
    use tracing::*;
    use tracing_test::traced_test;

    #[test]
    fn free_variables_shared() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
               "x" ; b!() ;= flip!(1/3);
               "l" ; b!() ;= sample!(var!("x"));
               "r" ; b!() ;= sample!(var!("x"));
               ...? ret ; b!()
            ])
        };
        check_approx("shared ", vec![1.0 / 3.0; 4], &mk(q!("l" x "r")), 10000);
    }

    // macro_rules! free_variable_2_tests {
    //     ($($name:ident: $value:expr,)*) => {
    //     $(
    //         #[test]
    //         fn $name() {
    //             let mk = |ret: ExprTyped| {
    //                 Program::Body(lets![
    //                     "x" ; b!() ;= flip!(1/3);
    //                     "y" ; b!() ;= sample!(
    //                         lets![
    //                             "x0" ; b!() ;= flip!(1/5);
    //                             ...? b!("x0" || "x") ; b!()
    //                         ]);
    //                    "_" ; b!() ;= observe!(b!("x" || "y")); // is this a problem?
    //                    ...? ret ; b!()
    //                 ])
    //             };
    //             ($value)(mk(q!("x" x "y")));
    //         }
    //     )*
    //     }
    // }

    // free_variable_2_tests! {
    //     free_variable_2_exact: (|p| check_exact("free_2/x*y", vec![0.714285714, 1.0, 1.0, 0.714285714], &p,)),
    //     // free_variable_2_approx: (|p| check_approx("free2/x*y", vec![0.714285714, 1.0, 1.0, 0.714285714], &p, 10,)),
    //     // free_variable_2_inv: (|p| check_invariant("free2/x*l ", None, None, &p)),
    // }

    // #[test]
    // #[traced_test]
    // fn free_variables_2() {
    //     let mk = |ret: ExprTyped| {
    //         Program::Body(lets![
    //             "x" ; b!() ;= flip!(1/3);
    //             "y" ; b!() ;= sample!(
    //                 lets![
    //                     "x0" ; b!() ;= flip!(1/5);
    //                     ...? b!("x0" || "x") ; b!()
    //                 ]);
    //            "_" ; b!() ;= observe!(b!("x" || "y")); // is this a problem?

    //            ...? ret ; b!()
    //         ])
    //     };
    //     let n = 10;
    //     debug!("{:#?}", &mk(b!("x" || "y")));
    //     // check_exact1("free2/x|y", 1.0, &mk(b!("x" || "y")));
    //     check_approx1("free2/x|y", 1.0, &mk(b!("x" || "y")), n);
    // }

    // #[test]
    // #[ignore = "also broken due to same reason as free_variables_0"]
    // // #[traced_test]
    // fn free_variables_shared_tuple() {
    //     let p = Program::Body(lets![
    //        "x" ; b!()     ;= flip!(1/3);
    //        "z" ; b!(B, B) ;= sample!(b!("x", "x"));
    //        ...? b!("z" ; b!(B, B)); b!(B, B)
    //     ]);
    //     check_approx("sharedtuple", vec![1.0 / 3.0, 1.0 / 3.0], &p, 1000);
    // }
    // #[test]
    // #[ignore = "I'm confident that this is a data flow analysis problem (in that sample formulas need to be added to the accepting criteria)"]
    // fn ite_3_with_one_sample_easy() {
    //     let mk = |ret: ExprTyped| {
    //         Program::Body(lets![
    //             "x" ; b!() ;= flip!(1/3);
    //             "y" ; b!() ;= ite!(
    //                 if ( var!("x") )
    //                 then { sample!(flip!(1/4)) }
    //                 else { flip!(1/5) });
    //             "_" ; b!() ;= observe!(b!("x" || "y"));
    //             ...? ret ; b!()
    //         ])
    //     };
    //     let n = 1000;
    //     check_approx1("ite_3/x  ", 0.714285714, &mk(b!("x")), n);
    //     check_approx1("ite_3/x|y", 1.000000000, &mk(b!("x" || "y")), n);
    // }

    // /// represents another semantic bug, still need to grapple how this all works, though
    // #[test]
    // #[ignore = "I'm confident that this is a data flow analysis problem (in that sample formulas need to be added to the accepting criteria)"]
    // // #[traced_test]
    // fn ite_3_with_one_sample_hard1() {
    //     let mk = |ret: ExprTyped| {
    //         Program::Body(lets![
    //             "x" ; b!() ;= flip!(2/3);
    //             "y" ; b!() ;= ite!(
    //                 if ( var!("x") )
    //                 then { sample!(flip!(3/4)) }
    //                 else { flip!(1/5) });
    //             "_" ; b!() ;= observe!(b!("x" || "y"));
    //             ...? ret ; b!()
    //         ])
    //     };
    //     let n = 1000;
    //     check_approx1("ite_3/y-sample3/4 ", 0.772727273, &mk(b!("y")), n); // dice's answer for 3/4 @ sample site
    //                                                                        // check_approx1("ite_3/y  ", 0.545454545, &mk(b!("y")), n); // dice's answer for 2/4 @ sample site
    // }
    // #[test]
    // #[ignore = "I'm confident that this is a data flow analysis problem (in that sample formulas need to be added to the accepting criteria)"]
    // fn ite_3_with_one_sample_hard2() {
    //     let mk = |ret: ExprTyped| {
    //         Program::Body(lets![
    //             "x" ; b!() ;= flip!(1/3);
    //             "y" ; b!() ;= ite!(
    //                 if ( var!("x") )
    //                 then { sample!(flip!(1/4)) }
    //                 else { flip!(1/5) });
    //             "_" ; b!() ;= observe!(b!("x" || "y"));
    //             ...? ret ; b!()
    //         ])
    //     };
    //     let n = 1000;
    //     check_approx1("ite_3/x&y", 0.178571429, &mk(b!("x" && "y")), n * n * n);
    // }
}
