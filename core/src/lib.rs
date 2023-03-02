#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)] // for Gamma : )
// temporary as I convert to using types
#![allow(unused_variables)]
// #![allow(clippy::clone_on_copy)]
// #![allow(clippy::type_complexity)]
// #![allow(clippy::redundant_clone)]
// #![allow(clippy::useless_format)]
// #![allow(clippy::needless_return)]
// #![allow(clippy::upper_case_acronyms)]
// #![allow(clippy::single_component_path_imports)]
// #![allow(clippy::enum_variant_names)]
// #![allow(clippy::match_like_matches_macro)]
// #![allow(clippy::let_and_return)]
// #![allow(clippy::len_zero)]
// #![allow(clippy::assign_op_pattern)]
// #![allow(clippy::unnecessary_cast)]
// #![allow(clippy::unnecessary_lazy_evaluations)]
// #![allow(clippy::too_many_arguments)]
// #![allow(clippy::ptr_arg)]
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

#[derive(Default)]
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
        let n = 5000;
        // debug_approx1("ite_3/x", 0.6, &mk(b!("x")), n); // works!
        // debug_approx1("ite_3/y", 0.3, &mk(b!("y")), n); // broken!
        check_approx1("ite_3/y", 0.3, &mk(b!("y")), n); // broken!

        // debug_approx1("ite_3/x|y", 0.7, &mk(b!("x" || "y")), n); // broken!
        // debug_approx1("ite_3/x&y", 0.2, &mk(b!("x" && "y")), n); // broken!
        // debug_approx("ite_3/x*y", vec![0.6, 0.3, 0.7, 0.2], &mk(q!("x" x "y")), n); // broken!
    }

    use std::collections::HashMap;
    #[test]
    #[ignore]
    #[traced_test]
    fn manual_ite() {
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
        // debug_approx1("ite_3/x", 0.6, &mk(b!("x")), n); // works!
        nfail_approx1("ite_3/y", 0.3, &mk(b!("y")), n); // broken!

        let mgr = Mgr::new_default_order(0);
        let names = HashMap::new();
        let (out, mgr) = formula::eval_with("x".to_string(), mgr, names).unwrap();
        // hand-derived
        let x = out.circuit;
        let (out, mut mgr) =
            formula::eval_with("(x & yt) | (!x & yf)".to_string(), mgr, out.names).unwrap();
        let y = out.circuit;
        let x_label = *out.names.get("x").unwrap();
        let yt_label = *out.names.get("yt").unwrap();
        let yf_label = *out.names.get("yf").unwrap();
        let yt = mgr.var(yt_label, true);
        let yf = mgr.var(yf_label, true);
        debug!("=======================================================================================");
        debug!(" current                                                                               ");
        debug!("=======================================================================================");

        debug!("x:  {}", x.print_bdd());
        debug!("y:  {}", y.print_bdd());
        debug!("yt: {}", yt.print_bdd());
        debug!("yf: {}", yf.print_bdd());

        let accept_true = mgr.and(x, yt);
        let accept_true = mgr.or(accept_true, x.neg());

        let dist_true = mgr.and(x.neg(), yf);
        let dist_true = mgr.or(x, dist_true);
        // conjoin query
        debug!("dist_true:    {}", dist_true.print_bdd());
        debug!("accept_true:  {}", accept_true.print_bdd());

        let accept_false = mgr.and(x, yt.neg());
        let accept_false = mgr.or(accept_false, x.neg());
        let dist_false = mgr.and(x.neg(), yf);
        // conjoin query
        debug!("dist_false:   {}", dist_false.print_bdd());
        debug!("accept_false: {}", accept_false.print_bdd());
        // let (out, mgr) =
        //     formula::eval_with("(x & F) | (!x & yf)".to_string(), mgr, out.names).unwrap();
        // let dist_false_comp = out.circuit;
        // debug!("dist_false_comp:   {}", dist_false_comp.print_bdd());
        // let (out, mgr) =
        //     formula::eval_with("(x & !yt) | (!x & T)".to_string(), mgr, out.names).unwrap();
        // let accept_false_comp = out.circuit;
        // debug!("accept_false_comp: {}", accept_false_comp.print_bdd());

        let var_order = mgr.get_order().clone();
        let mut params = WmcParams::new(0.0, 1.0);
        for (lbl, weight) in [
            (yf_label, Weight::new(0.7500, 0.2500)),
            (yt_label, Weight::new(0.6667, 0.3333)),
            (x_label, Weight::new(0.4000, 0.6000)),
        ] {
            params.set_weight(lbl, weight.lo, weight.hi);
        }
        let w_true = calculate_wmc_prob_hf64(&mut mgr, &params, &var_order, dist_true, accept_true);
        debug!("w_true:   {:.3}", w_true);
        let w_false =
            calculate_wmc_prob_hf64(&mut mgr, &params, &var_order, dist_false, accept_false);
        debug!("w_false:   {:.3}", w_false);
        debug!(
            "1*w_true + 2*w_false / 3  == 0.3?   {:.4}",
            (1.0 * w_true + 2.0 * w_false) / 3.0
        );
        debug!("=======================================================================================");
        debug!(" exact                                                                                 ");
        debug!("=======================================================================================");

        let w_exact_t =
            calculate_wmc_prob_hf64(&mut mgr, &params, &var_order, dist_true, BddPtr::PtrTrue);
        debug!("w_exact_t:   {:.3}", w_exact_t);

        let w_exact_f =
            calculate_wmc_prob_hf64(&mut mgr, &params, &var_order, dist_false, BddPtr::PtrTrue);
        debug!("w_exact_f:   {:.3}", w_exact_f);
        debug!(
            "1*w_exact_t + 2*w_exact_f / 3 == 0.3?   {:.4}",
            (1.0 * w_exact_t + 2.0 * w_exact_f) / 03.0
        );
        debug!("=======================================================================================");
        debug!(" hypothesized                                                                          ");
        debug!("=======================================================================================");
        let dist_hyp_true = dist_true.clone();
        let dist_hyp_false = dist_false.clone();
        debug!("dist_hyp_true:    {}", dist_hyp_true.print_bdd());
        debug!("dist_hyp_false:   {}", dist_hyp_false.print_bdd());

        let accept_hyp_true = mgr.and(x, yt);
        let accept_hyp_true_tmp = mgr.and(x.neg(), yt);
        let accept_hyp_true = mgr.or(accept_hyp_true, accept_hyp_true_tmp);
        debug!("accept_hyp_true:  {}", accept_hyp_true.print_bdd());

        let accept_hyp_false = mgr.and(x, yt.neg());
        let accept_hyp_false_tmp = mgr.and(x.neg(), yt.neg());
        let accept_hyp_false = mgr.or(accept_hyp_false, accept_hyp_false_tmp);
        debug!("accept_hyp_false:  {}", accept_hyp_false.print_bdd());

        let w_hyp_t = calculate_wmc_prob_hf64(
            &mut mgr,
            &params,
            &var_order,
            dist_hyp_true,
            accept_hyp_true,
        );
        debug!("w_hyp_t:   {:.3}", w_hyp_t);

        let w_hyp_f = calculate_wmc_prob_hf64(
            &mut mgr,
            &params,
            &var_order,
            dist_hyp_false,
            accept_hyp_false,
        );
        debug!("w_hyp_f:   {:.3}", w_hyp_f);
        debug!(
            "1*w_hyp_t + 2*w_hyp_f / 3 == 0.3?   {:.4}",
            (1.0 * w_hyp_t + 2.0 * w_hyp_f) / 03.0
        );
        // let (out, mgr) =
        //     formula::eval_with("(x & yt) | (!x & T)".to_string(), mgr, out.names).unwrap();
        // let accept_true_comp = out.circuit;
        // debug!("accept_true_comp:  {}", accept_true_comp.print_bdd());

        todo!()

        // debug_approx1("ite_3/x|y", 0.7, &mk(b!("x" || "y")), n); // broken!
        // debug_approx1("ite_3/x&y", 0.2, &mk(b!("x" && "y")), n); // broken!
        // debug_approx("ite_3/x*y", vec![0.6, 0.3, 0.7, 0.2], &mk(q!("x" x "y")), n); // broken!
    }

    #[test]
    // #[ignore]
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
    // #[ignore]
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
    //#[ignore]
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
        check_approx1("ite_3/observe/x  ", 0.909090909, &mk(b!("x")), n);
        check_approx1("ite_3/observe/y  ", 0.318181818, &mk(b!("y")), n);
        check_approx1("ite_3/observe/x|y", 1.000000000, &mk(b!("x" || "y")), n);
        check_approx1("ite_3/observe/x&y", 0.227272727, &mk(b!("x" && "y")), n);

        // dice's answer for 2/4 @ sample site
        // check_approx1("ite_3/y-sample2/4  ", 0.545454545, &mk(b!("y")), n);
        // dice's answer for 3/4 @ sample site
        // check_approx1("ite_3/y-sample3/4 ", 0.772727273, &mk(b!("y")), n);

        // last one to tackle:
        // dice's answer for 1/4 @ sample site
        // check_approx1("ite_3/x&y", 0.227272727, &mk(b!("x" && "y")), n * n * n);
    }

    #[test]
    // #[traced_test]
    #[ignore]
    fn ite_3_with_one_sample_hard1_extra() {
        let mk = |ret: ExprTyped| {
            program!(lets![
                "x" ; b!() ;= flip!(2/3);
                "w" ; b!() ;= flip!(2/7);
                "y" ; b!() ;= ite!(
                    if ( var!("x") )
                        then { sample!(lets![
                                 "q" ; b!() ;= flip!(1/4);
                                 "_" ; b!() ;= observe!(b!("q" || "w"));
                                 ...? b!("q") ; b!()
                        ]) }
                    else { flip!(1/5) });
                "_" ; b!() ;= observe!(b!("x" || "y"));
                ...? ret ; b!()
            ])
        };
        let n = 50000;
        debug_approx1("ite_3/observe/y  ", 0.620253165, &mk(b!("y")), n);

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
    // #[ignore]
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
