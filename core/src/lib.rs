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
// mod collect_weightmap;
mod compile;
#[cfg(test)]
mod debug;
mod inference;
mod parser;
mod render;
#[cfg(test)]
mod tests;
mod typecheck;
mod uniquify;
mod utils;

use crate::annotate::LabelEnv;
use crate::compile::{compile, CompileError, Compiled, Env, Output, Result};
use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
use crate::typecheck::typecheck;
use crate::uniquify::SymEnv;

pub fn run(env: &mut Env, p: &ProgramTyped) -> Result<Output> {
    let p = typecheck(p)?;
    let mut senv = SymEnv::default();
    let p = senv.uniquify(&p)?;
    let mut lenv = LabelEnv::new();
    let (p, vo, varmap, inv, mxlbl) = lenv.annotate(&p)?;
    env.names = senv.names; // just for debugging, really.
    env.order = Some(vo);
    env.max_label = Some(mxlbl);
    env.varmap = Some(varmap);
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

    macro_rules! ite_3_with_one_sample_tests {
    ($($name:ident: $query:expr, $value:expr,)*) => {
    $(
        #[test]
        #[ignore]
        fn $name() {
            let mk = |ret: ExprTyped| {
                Program::Body(lets![
                    "x" ; b!() ;= flip!(2/3);
                    "y" ; b!() ;= ite!(
                        if ( var!("x") )
                        then { sample!(flip!(1/4)) }
                        else { flip!(1/5) });
                    "_" ; b!() ;= observe!(b!("x" || "y"));
                    ...? ret ; b!()
                ])
            };
            ($value)(mk($query));
        }
    )*
    }
}

    ite_3_with_one_sample_tests! {
        ite_3_with_one_sample_easy_x:  b!("x"), (|p| check_approx1("ite_3/x  ", 0.909090909, &p, 1000)),
        ite_3_with_one_sample_easy_x_or_y: b!("x" || "y"), (|p| check_approx1("ite_3/x|y", 1.000000000, &p, 1000)),
    }

    #[test]
    #[traced_test]
    #[ignore]
    fn ite_3_with_one_sample_hard1_simplified() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "x" ; b!() ;= flip!(1/5);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                    then { sample!(flip!(1/3)) }
                    else { flip!(1/4) });
                ...? ret ; b!()
            ])
        };
        let n = 10;
        // check_exact1("ite_3/y-sample1/4-simpl", 0.266666667, &mk(b!("y")));
        check_approx1("ite_3/y-sample1/4-simpl", 0.266666667, &mk(b!("y")), n);
        // dice's answer for 2/4 @ sample site
        // check_approx1("ite_3/y-sample2/4  ", 0.545454545, &mk(b!("y")), n);
        // dice's answer for 3/4 @ sample site
        // check_approx1("ite_3/y-sample3/4 ", 0.772727273, &mk(b!("y")), n);

        // last one to tackle:
        // dice's answer for 1/4 @ sample site
        // check_approx1("ite_3/x&y", 0.227272727, &mk(b!("x" && "y")), n * n * n);
    }

    // /// represents another semantic bug, still need to grapple how this all works, though
    // #[test]
    // // #[ignore]
    // // #[traced_test]
    // fn ite_3_with_one_sample_hard1() {
    //     let mk = |ret: ExprTyped| {
    //         Program::Body(lets![
    //             "x" ; b!() ;= flip!(2/3);
    //             "y" ; b!() ;= ite!(
    //                 if ( var!("x") )
    //                 then { sample!(flip!(1/4)) }
    //                 else { flip!(1/5) });
    //             "_" ; b!() ;= observe!(b!("x" || "y"));
    //             ...? ret ; b!()
    //         ])
    //     };
    //     let n = 50000;
    //     check_approx1("ite_3/y-sample1/4 ", 0.464285714, &mk(b!("y")), n);
    //     // dice's answer for 2/4 @ sample site
    //     // check_approx1("ite_3/y-sample2/4  ", 0.545454545, &mk(b!("y")), n);
    //     // dice's answer for 3/4 @ sample site
    //     // check_approx1("ite_3/y-sample3/4 ", 0.772727273, &mk(b!("y")), n);

    //     // last one to tackle:
    //     // dice's answer for 1/4 @ sample site
    //     // check_approx1("ite_3/x&y", 0.227272727, &mk(b!("x" && "y")), n * n * n);
    // }

    #[test]
    ////#[traced_test]
    fn free_variable_2_approx() {
        let mk = |ret: ExprTyped| {
            Program::Body(lets![
                "x" ; b!() ;= flip!(1/3);
                "y" ; b!() ;= sample!(
                    lets![
                        "x0" ; b!() ;= flip!(1/5);
                        ...? b!("x0" || "x") ; b!()
                    ]);
               "_" ; b!() ;= observe!(b!("x" || "y")); // is this a problem?
               ...? ret ; b!()
            ])
        };
        // check_approx("free2/x*y", vec![0.714285714, 1.0, 1.0, 0.714285714], &mk(q!("x" x "y")), 10,);
        check_approx1("free2/x", 0.714285714, &mk(b!("x")), 10000);
        check_approx1("free2/x&y", 0.714285714, &mk(b!("x" && "y")), 10000);
        //check_approx("free2/x&y", vec![0.714285714, 1.0, 1.0, 0.714285714], &mk(q!("x" x "y")), 10,);
    }
}
