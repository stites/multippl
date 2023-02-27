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
mod grammar;
mod grammar_macros;

mod annotate;
// mod collect_weightmap;
mod compile;
mod inference;
mod parser;
mod render;
#[cfg(test)]
mod tests;
mod typecheck;
mod uniquify;
mod utils;

pub struct Options {
    seed: Option<u64>,
    debug: bool, // overrides seed
}
impl Options {
    pub fn rng(&self) -> StdRng {
        match self.seed {
            None => rand::SeedableRng::from_entropy(),
            Some(s) => rand::SeedableRng::seed_from_u64(s),
        }
    }
    pub fn stoch() -> Self {
        Default::default()
    }
    pub fn seed(s: u64) -> Self {
        Self {
            seed: Some(s),
            ..Default::default()
        }
    }
    pub fn debug() -> Self {
        Self {
            debug: true,
            ..Default::default()
        }
    }
}
impl Default for Options {
    fn default() -> Self {
        Self {
            debug: false,
            seed: None,
        }
    }
}
use crate::annotate::LabelEnv;
use crate::compile::{compile, CompileError, Compiled, Env, Mgr, Output, Result};
use crate::typecheck::{
    grammar::{ExprTyped, ProgramTyped},
    typecheck,
};
use crate::uniquify::SymEnv;
use rand::rngs::StdRng;

pub fn run(p: &ProgramTyped) -> Result<(Mgr, Output)> {
    let (m, c) = runner(p, &Default::default())?;
    Ok((m, c.as_output().unwrap()))
}
pub fn run_h(p: &ProgramTyped, mgr: &mut Mgr) -> Result<Output> {
    let c = runner_h(p, mgr, &Default::default())?;
    Ok(c.as_output().unwrap())
}

pub fn runner(p: &ProgramTyped, opt: &Options) -> Result<(Mgr, Compiled)> {
    let mut mgr = make_mgr(p);
    let c = runner_h(p, &mut mgr, opt)?;
    Ok((mgr, c))
}

pub fn make_mgr(p: &ProgramTyped) -> Mgr {
    let Ok(p) = typecheck(p) else { todo!() };
    let mut senv = SymEnv::default();
    let Ok(p) = senv.uniquify(&p) else { todo!() };
    let mut lenv = LabelEnv::new();
    let Ok((p, vo, varmap, inv, mxlbl)) = lenv.annotate(&p) else { todo!() };

    Mgr::new_default_order(mxlbl as usize)
}

pub fn runner_h(p: &ProgramTyped, mgr: &mut Mgr, opt: &Options) -> Result<Compiled> {
    let p = typecheck(p)?;
    let mut senv = SymEnv::default();
    let p = senv.uniquify(&p)?;
    let mut lenv = LabelEnv::new();
    let (p, vo, varmap, inv, mxlbl) = lenv.annotate(&p)?;

    let mut rng = opt.rng();
    let orng = if opt.debug { None } else { Some(&mut rng) };
    let mut env = Env::new(mgr, orng);

    env.varmap = Some(varmap);
    env.inv = Some(inv);

    let c = compile(&mut env, &p)?;
    tracing::debug!("hurray!");
    Ok(c)
}

#[cfg(test)]
mod active_tests {
    use super::*;
    use crate::annotate::LabelEnv;
    use crate::compile::*;
    use crate::compile::{compile, CompileError, Compiled, Env, Output, Result};
    use crate::grammar::*;
    use crate::grammar_macros::*;
    use crate::typecheck::{grammar::ExprTyped, typecheck};
    use crate::uniquify::SymEnv;
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
    // #[ignore]
    fn ite_3_with_one_sample_hard1_simplified_even_more_true() {
        let p = {
            program!(ite!(
                    if ( b!(true) )
                    then { sample!(flip!(1/3)) }
                    else { flip!(1/4) }))
        };
        let n = 5000;
        check_approx1("ite_3/y-sample1/4-simpl", 1.0 / 3.0, &p, n);
        // check_approx1("ite_3/y-sample1/4-simpl", 0.2, &mk(b!("x")), n);
    }
    #[test]
    // #[ignore]
    // #[traced_test]
    fn ite_3_with_one_sample_hard1_simplified_even_more_false() {
        let p = {
            program!(ite!(
                    if ( b!(false) )
                    then { sample!(flip!(1/3)) }
                    else { flip!(1/4) }))
        };
        let n = 5000;
        check_approx1("ite_3/y-sample1/4-simpl", 1.0 / 4.0, &p, n);
        // check_approx1("ite_3/y-sample1/4-simpl", 0.2, &mk(b!("x")), n);
    }
    #[test]
    #[ignore]
    fn ite_3_with_one_sample_hard1_simplified_more() {
        let mk = |ret: ExprTyped| {
            program!(lets![
                "x" ; b!() ;= flip!(3/5);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                    then { sample!(flip!(1/3)) }
                    else { flip!(1/4) });
                ...? ret ; b!()
            ])
        };
        let n = 10000;
        check_approx1("ite_3/y-sample1/4-simpl", 0.3, &mk(b!("y")), n);
        // last one to tackle:
        // dice's answer for 1/4 @ sample site
        // check_approx1("ite_3/x&y", 0.227272727, &mk(b!("x" && "y")), n * n * n);
    }

    #[test]
    #[ignore]
    // #[traced_test]
    fn ite_3_with_one_sample_hard1_simplified() {
        let mk = |ret: ExprTyped| {
            program!(lets![
                "x" ; b!() ;= flip!(1/5);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                    then { sample!(flip!(1/3)) }
                    else { flip!(1/4) });
                ...? ret ; b!()
            ])
        };
        let n = 5000;
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

    #[test]
    #[ignore]
    fn ite_3_with_one_sample_easy_x() {
        let mk = |ret: ExprTyped| {
            program!(lets![
                "x" ; b!() ;= flip!(2/3);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                    then { sample!(flip!(1/4)) }
                    else { flip!(1/5) });
                "_" ; b!() ;= observe!(b!("x" || "y"));
                ...? ret ; b!()
            ])
        };
        check_approx1("ite_3/x  ", 0.909090909, &mk(b!("x")), 1000);
    }

    /// represents another semantic bug, still need to grapple how this all works, though
    #[test]
    // #[traced_test]
    #[ignore]
    fn ite_3_with_one_sample_hard1() {
        let mk = |ret: ExprTyped| {
            program!(lets![
                "x" ; b!() ;= flip!(2/3);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                    then { sample!(flip!(1/4)) }
                    else { flip!(1/5) });
                "_" ; b!() ;= observe!(b!("x" || "y"));
                ...? ret ; b!()
            ])
        };
        let n = 50000;
        check_approx1("ite_3/y-sample1/4 ", 0.464285714, &mk(b!("y")), n);
        // dice's answer for 2/4 @ sample site
        // check_approx1("ite_3/y-sample2/4  ", 0.545454545, &mk(b!("y")), n);
        // dice's answer for 3/4 @ sample site
        // check_approx1("ite_3/y-sample3/4 ", 0.772727273, &mk(b!("y")), n);

        // last one to tackle:
        // dice's answer for 1/4 @ sample site
        // check_approx1("ite_3/x&y", 0.227272727, &mk(b!("x" && "y")), n * n * n);
    }

    /// passing
    #[test]
    #[ignore]
    fn ite_3_with_one_sample_easy_x_or_y() {
        let mk = |ret: ExprTyped| {
            program!(lets![
                "x" ; b!() ;= flip!(2/3);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                    then { sample!(flip!(1/4)) }
                    else { flip!(1/5) });
                "_" ; b!() ;= observe!(b!("x" || "y"));
                ...? ret ; b!()
            ])
        };
        check_approx1("ite_3/x|y", 1.000000000, &mk(b!("x" || "y")), 1000);
    }

    /// working
    #[test]
    fn free_variable_2_approx() {
        let mk = |ret: ExprTyped| {
            program!(lets![
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
        check_approx(
            "free2/x*y",
            vec![0.714285714, 1.0, 1.0, 0.714285714],
            &mk(q!("x" x "y")),
            10000,
        );
        check_approx1("free2/x", 0.714285714, &mk(b!("x")), 10000);
        check_approx1("free2/x&y", 0.714285714, &mk(b!("x" && "y")), 10000);
    }
}
