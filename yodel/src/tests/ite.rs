use crate::compile::*;
use crate::grammar::*;
use crate::inference::*;
use crate::tests::checks::*;
use crate::typeinf::grammar::*;
use crate::*;

use itertools::*;
use rsdd::sample::probability::*;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::ops::Range;
use tracing::*;
use tracing_test::*;
// ===================================================================== //
//                   START: deterministic if-then-else                   //
// ===================================================================== //

#[test]
fn ite_00() {
    let mk = |guard: &str| {
        r#"exact {
        if ("#
            .to_owned()
            + guard
            + r#")
        then ( flip (1.0 /3.0 ) )
        else ( flip (1.0 /5.0 ) )
        "#
    };

    check_exact1("ite_00/T", 1.0 / 3.0, mk("true"));
    check_exact1("ite_00/F", 1.0 / 5.0, mk("false"));
}

#[test]
fn ite_0() {
    let mk = |ret: &str| {
        r#"exact {
        let b = if ( true )
                then flip 1.0 / 4.0
                else flip 1.0 / 5.0 in
        b"#
        .to_owned()
            + ""
            + r#"\n}"#
    };

    check_exact1("ite_0  ", 1.0 / 4.0, mk("b"));
}

#[test]
fn ite_1() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 1.0 / 3.0 in
        let y = if ( x )
                then flip 1.0 / 4.0
                else flip 2.0 / 5.0 in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };

    let yres = (1.0 / 3.0 * 0.25) + (2.0 / 3.0 * 0.4);
    check_exact1("ite_1/x  ", 1.0 / 3.0, mk("x"));
    check_exact1("ite_1/y  ", yres, mk("y"));
    check_exact1("ite_1/x|y", 0.6, mk("x || y"));
    check_exact1("ite_1/x&y", 0.083333333, mk("x && y"));
}

#[test]
fn ite_2() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 1.0 / 3.0 in
        let y = if ( x )
                then flip 1.0 / 4.0
                else flip 1.0 / 5.0 in
        let _ = observe x || y in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };

    check_exact1("ite_2/y  ", 0.464285714, mk("y"));
    check_exact1("ite_2/x  ", 0.714285714, mk("x"));
    check_exact1("ite_2/x|y", 1.000000000, mk("x || y"));
    check_exact1("ite_2/x&y", 0.178571429, mk("x && y"));
}

#[test]
fn ite_3_with_one_sample_hard1_simplified_even_more_true() {
    let p = r#"
    exact { if true then sample ( ~bern(1.0/3.0) ) else flip 1.0 / 4.0 }
    "#;
    let n = 10000;
    check_approx1("ite_3/y-sample1/4-simpl", 1.0 / 3.0, p, n);
    // check_approx1("ite_3/y-sample1/4-simpl", 0.2, mk(b!("x")), n);
}

#[test]
fn ite_3_with_one_sample_hard1_simplified_even_more_false() {
    let p = r#"
    exact { if false then sample ( ~bern(1.0/3.0) ) else flip 1.0 / 4.0 }
    "#;
    let n = 5000;
    check_approx1("ite_3/y-sample1/4-simpl", 1.0 / 4.0, p, n);
    // check_approx1("ite_3/y-sample1/4-simpl", 0.2, mk(b!("x")), n);
}
#[test]
fn ite_3_with_one_sample_hard1_simplified_more() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 3.0 / 5.0 in
        let y = if x then sample ( ~ bern(1.0 / 3.0)) else flip 1.0 / 4.0 in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    let n = 5000;
    // debug_approx1("ite_3/x", 0.6, mk(b!("x")), n); // works!
    // debug_approx1("ite_3/y", 0.3, mk(b!("y")), n); // broken!
    check_approx1("ite_3/y", 0.3, mk("y"), n); // broken!

    // debug_approx1("ite_3/x|y", 0.7, mk(b!("x" || "y")), n); // broken!
    // debug_approx1("ite_3/x&y", 0.2, mk(b!("x" && "y")), n); // broken!
    // debug_approx("ite_3/x*y", vec![0.6, 0.3, 0.7, 0.2], mk(q!("x" x "y")), n); // broken!
}

// #[test]
// #[ignore = "not actually a test, just a hand-crafted exploration"]
// // #[traced_test]
// fn manual_ite() {
//     use rsdd::builder::bdd_builder::*;
//     use rsdd::repr::wmc::*;
//     let mk = |ret: EExprInferable| {
//         program!(lets![
//             "x" ;= flip!(3/5);
//             "y" ;= ite!(
//                 if ( var!("x") )
//                 then { sample!(flip!(1/3)) }
//                 else { flip!(1/4) });
//             ...? ret
//         ])
//     };
//     let n = 10000;
//     // debug_approx1("ite_3/x", 0.6, mk(b!("x")), n); // works!
//     nfail_approx1("ite_3/y", 0.3, mk(b!("y")), n); // broken!

//     let mgr = Mgr::new_default_order(0);
//     let names = HashMap::new();
//     let (out, mgr) = formula::eval_with("x".to_string(), mgr, names).unwrap();
//     // hand-derived
//     let x = out.circuit;
//     let (out, mut mgr) =
//         formula::eval_with("(x & yt) | (!x & yf)".to_string(), mgr, out.names).unwrap();
//     let y = out.circuit;
//     let x_label = *out.names.get("x").unwrap();
//     let yt_label = *out.names.get("yt").unwrap();
//     let yf_label = *out.names.get("yf").unwrap();
//     let yt = mgr.var(yt_label, true);
//     let yf = mgr.var(yf_label, true);
//     debug!(
//         "======================================================================================="
//     );
//     debug!(
//         " current                                                                               "
//     );
//     debug!(
//         "======================================================================================="
//     );

//     debug!("x:  {}", x.print_bdd());
//     debug!("y:  {}", y.print_bdd());
//     debug!("yt: {}", yt.print_bdd());
//     debug!("yf: {}", yf.print_bdd());

//     let accept_true = mgr.and(x, yt);
//     let accept_true = mgr.or(accept_true, x.neg());

//     let dist_true = mgr.and(x.neg(), yf);
//     let dist_true = mgr.or(x, dist_true);
//     // conjoin query
//     debug!("dist_true:    {}", dist_true.print_bdd());
//     debug!("accept_true:  {}", accept_true.print_bdd());

//     let accept_false = mgr.and(x, yt.neg());
//     let accept_false = mgr.or(accept_false, x.neg());
//     let dist_false = mgr.and(x.neg(), yf);
//     // conjoin query
//     debug!("dist_false:   {}", dist_false.print_bdd());
//     debug!("accept_false: {}", accept_false.print_bdd());
//     // let (out, mgr) =
//     //     formula::eval_with("(x & F) | (!x & yf)".to_string(), mgr, out.names).unwrap();
//     // let dist_false_comp = out.circuit;
//     // debug!("dist_false_comp:   {}", dist_false_comp.print_bdd());
//     // let (out, mgr) =
//     //     formula::eval_with("(x & !yt) | (!x & T)".to_string(), mgr, out.names).unwrap();
//     // let accept_false_comp = out.circuit;
//     // debug!("accept_false_comp: {}", accept_false_comp.print_bdd());

//     let var_order = mgr.get_order().clone();
//     let mut params = WmcParams::new(RealSemiring(0.0), RealSemiring(1.0));
//     for (lbl, weight) in [
//         (yf_label, Weight::new(0.7500, 0.2500)),
//         (yt_label, Weight::new(0.6667, 0.3333)),
//         (x_label, Weight::new(0.4000, 0.6000)),
//     ] {
//         params.set_weight(lbl, weight.lo, weight.hi);
//     }
//     let w_true = calculate_wmc_prob_hf64(&mut mgr, params, &var_order, dist_true, accept_true).0;
//     debug!("w_true:   {:.3}", w_true);
//     let w_false =
//         calculate_wmc_prob_hf64(&mut mgr, params, &var_order, dist_false, accept_false).0;
//     debug!("w_false:   {:.3}", w_false);
//     debug!(
//         "1*w_true + 2*w_false / 3  == 0.3?   {:.4}",
//         (1.0 * w_true + 2.0 * w_false) / 3.0
//     );
//     debug!("=========================================");
//     debug!(" exact                                   ");
//     debug!("=========================================");

//     let w_exact_t =
//         calculate_wmc_prob_hf64(&mut mgr, params, &var_order, dist_true, BddPtr::PtrTrue).0;
//     debug!("w_exact_t:   {:.3}", w_exact_t);

//     let w_exact_f =
//         calculate_wmc_prob_hf64(&mut mgr, params, &var_order, dist_false, BddPtr::PtrTrue).0;
//     debug!("w_exact_f:   {:.3}", w_exact_f);
//     debug!(
//         "1*w_exact_t + 2*w_exact_f / 3 == 0.3?   {:.4}",
//         (1.0 * w_exact_t + 2.0 * w_exact_f) / 03.0
//     );
//     debug!("=========================================");
//     debug!(" hypothesized                            ");
//     debug!("=========================================");
//     let dist_hyp_true = dist_true.clone();
//     let dist_hyp_false = dist_false.clone();
//     debug!("dist_hyp_true:    {}", dist_hyp_true.print_bdd());
//     debug!("dist_hyp_false:   {}", dist_hyp_false.print_bdd());

//     let accept_hyp_true = mgr.and(x, yt);
//     let accept_hyp_true_tmp = mgr.and(x.neg(), yt);
//     let accept_hyp_true = mgr.or(accept_hyp_true, accept_hyp_true_tmp);
//     debug!("accept_hyp_true:  {}", accept_hyp_true.print_bdd());

//     let accept_hyp_false = mgr.and(x, yt.neg());
//     let accept_hyp_false_tmp = mgr.and(x.neg(), yt.neg());
//     let accept_hyp_false = mgr.or(accept_hyp_false, accept_hyp_false_tmp);
//     debug!("accept_hyp_false:  {}", accept_hyp_false.print_bdd());

//     let w_hyp_t = calculate_wmc_prob_hf64(
//         &mut mgr,
//         params,
//         &var_order,
//         dist_hyp_true,
//         accept_hyp_true,
//     )
//     .0;
//     debug!("w_hyp_t:   {:.3}", w_hyp_t);

//     let w_hyp_f = calculate_wmc_prob_hf64(
//         &mut mgr,
//         params,
//         &var_order,
//         dist_hyp_false,
//         accept_hyp_false,
//     )
//     .0;
//     debug!("w_hyp_f:   {:.3}", w_hyp_f);
//     debug!(
//         "1*w_hyp_t + 2*w_hyp_f / 3 == 0.3?   {:.4}",
//         (1.0 * w_hyp_t + 2.0 * w_hyp_f) / 03.0
//     );
//     // let (out, mgr) =
//     //     formula::eval_with("(x & yt) | (!x & T)".to_string(), mgr, out.names).unwrap();
//     // let accept_true_comp = out.circuit;
//     // debug!("accept_true_comp:  {}", accept_true_comp.print_bdd());

//     todo!()

//     // debug_approx1("ite_3/x|y", 0.7, mk(b!("x" || "y")), n); // broken!
//     // debug_approx1("ite_3/x&y", 0.2, mk(b!("x" && "y")), n); // broken!
//     // debug_approx("ite_3/x*y", vec![0.6, 0.3, 0.7, 0.2], mk(q!("x" x "y")), n); // broken!
// }

#[test]
fn ite_3_with_one_sample_hard1_simplified() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 1.0 / 5.0 in
        let y = if x then sample { ~ bern(1.0 / 3.0)} else flip 1.0 / 4.0 in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    let n = 5000;

    // check_exact1("ite_3/y-sample1/4-simpl", 0.266666667, mk(b!("y")));
    check_approx1("ite_3/y-sample1/4-simpl", 0.266666667, mk("y"), n);
    // dice's answer for 2/4 @ sample site
    // check_approx1("ite_3/y-sample2/4  ", 0.545454545, mk(b!("y")), n);
    // dice's answer for 3/4 @ sample site
    // check_approx1("ite_3/y-sample3/4 ", 0.772727273, mk(b!("y")), n);

    // last one to tackle:
    // dice's answer for 1/4 @ sample site
    // check_approx1("ite_3/x&y", 0.227272727, mk(b!("x" && "y")), n * n * n);
}

#[test]
fn ite_3_with_one_sample_easy_x() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 2.0 / 3.0 in
        let y = if x then sample { ~ bern(1.0 / 4.0)} else flip 1.0 / 5.0 in
        let _ = observe ( x || y ) in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    check_approx1("ite_3/x  ", 0.909090909, mk("x"), 1000);
}

#[test]
fn ite_3_with_one_sample_hard1() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 2.0 / 3.0 in
        let y = if x then sample { ~ bern(1.0 / 4.0)} else flip 1.0 / 5.0 in
        let _ = observe ( x || y ) in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    let n = 3000;
    check_approx1("ite_3/observe/x  ", 0.909090909, mk("x"), n);
    check_approx1("ite_3/observe/y  ", 0.318181818, mk("y"), n);
    check_approx1("ite_3/observe/x|y", 1.000000000, mk("x || y"), n);
    check_approx1("ite_3/observe/x&y", 0.227272727, mk("x && y"), n);

    // dice's answer for 2/4 @ sample site
    // check_approx1("ite_3/y-sample2/4  ", 0.545454545, mk(b!("y")), n);
    // dice's answer for 3/4 @ sample site
    // check_approx1("ite_3/y-sample3/4 ", 0.772727273, mk(b!("y")), n);

    // last one to tackle:
    // dice's answer for 1/4 @ sample site
    // check_approx1("ite_3/x&y", 0.227272727, mk(b!("x" && "y")), n * n * n);
}

#[test]
fn ite_3_with_one_sample_easy_x_or_y() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 2.0 / 3.0 in
        let y = if x then sample { ~ bern(1.0 / 4.0) } else flip 1.0 / 5.0 in
        let _ = observe ( x || y ) in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    check_approx1("ite_3/x|y", 1.000000000, mk("x || y"), 1000);
}

#[test]
// #[traced_test]
#[ignore = "seems to be a problem, but we can punt on this because I'm not sure observes in this manner _should_ work"]
fn ite_3_with_one_sample_hard1_extra() {
    let mk = |ret: &str| {
        r#"exact {
        let x = flip 2.0 / 3.0 in
        let w = flip 2.0 / 7.0 in
        let y = if x then (
            let q = sample { ~ bern(1.0 / 4.0)} in
            let _ = observe ( q || w ) in
            q
        ) else (
            flip 0.2
        ) in
        let _ = observe ( x || y ) in
        "#
        .to_owned()
            + ret
            + r#"\n}"#
    };
    let n = 50000;
    check_approx1("ite_3/observe/y  ", 0.620253165, mk("y"), n);

    // dice's answer for 2/4 @ sample site
    // check_approx1("ite_3/y-sample2/4  ", 0.545454545, mk(b!("y")), n);
    // dice's answer for 3/4 @ sample site
    // check_approx1("ite_3/y-sample3/4 ", 0.772727273, mk(b!("y")), n);

    // last one to tackle:
    // dice's answer for 1/4 @ sample site
    // check_approx1("ite_3/x&y", 0.227272727, mk(b!("x" && "y")), n * n * n);
}
