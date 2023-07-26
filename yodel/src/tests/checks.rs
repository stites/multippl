use crate::utils::render::*;

use crate::compile::*;
use crate::grammar::*;
use crate::inference::*;
use crate::typeinf::grammar::*;
use crate::*;
use itertools::*;
use tracing::*;

const USE_OPT: bool = false;
const USE_DEBUG: bool = false;

pub fn check_invariant(s: &str, precision: Option<f64>, n: Option<usize>, p: &str) {
    let precision = precision.unwrap_or_else(|| 0.01);
    let n = n.unwrap_or_else(|| 10000);
    let exact = inference::exact(&p);
    let (approx, _) = importance_weighting_h(
        n,
        p,
        &Options {
            opt: USE_OPT,
            debug: USE_DEBUG,
            // seed: Some(9),
            ..Default::default()
        },
    );
    debug!("exact:  {:?}", exact);
    debug!("approx: {:?}", approx);

    assert_eq!(
        exact.len(),
        approx.len(),
        "[check_inv][{s}][mismatch shape] compiled exact queries {}, but approx returned results {}",
        renderfloats(&exact, false),
        renderfloats(&approx, false),
    );
    izip!(exact, approx)
        .enumerate()
        .for_each(|(i, (ext, apx))| {
            let ret = (ext - apx).abs() < precision;
            let i = i + 1;
            assert!(
                ret,
                "[check_inv][{s}#{i}][err]((exact: {ext}) - (approx: {apx})).abs < {precision}"
            );
        });
}

pub fn check_inference(
    infname: &str,
    inf: &dyn Fn(&str) -> (Vec<f64>, Option<WmcStats>),
    precision: f64,
    s: &str,
    fs: Vec<f64>,
    p: &str,
) {
    check_inference_h(infname, inf, precision, s, fs, p, true)
}

pub fn check_inference_h(
    infname: &str,
    inf: &dyn Fn(&str) -> (Vec<f64>, Option<WmcStats>),
    precision: f64,
    s: &str,
    fs: Vec<f64>,
    p: &str,
    do_assert: bool,
) {
    let prs = inf(p).0;
    assert_eq!(
        prs.len(),
        fs.len(),
        "[check_{infname}][{s}] check_inference compiled queries {}, tests expect results {}",
        renderfloats(&prs, false),
        renderfloats(&fs, false),
    );
    // println!("query: {:?}", p.query());
    println!("expecting: {}", renderfloats(&fs, false));
    println!("computed:  {}", renderfloats(&prs, false));
    izip!(prs, fs).enumerate().for_each(|(i, (pr, f))| {
        let ret = (f - pr).abs() < precision;
        let i = i + 1;
        if do_assert {
            assert!(
                ret,
                "[check_{infname}][{s}#{i}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
            );
        } else {
            debug!("[check_{infname}][{s}#{i}][{ret}]((expected: {f}) - (actual: {pr})).abs < {precision}: {ret}");
        }
    });
}

pub fn check_exact(s: &str, f: Vec<f64>, p: &str) {
    debug!("program:\n{}", &p);
    check_inference("exact", &inference::exact_with_h, 0.000001, s, f, &p);
}
pub fn check_exact1(s: &str, f: f64, p: &str) {
    check_exact(s, vec![f], p)
}
pub fn check_approx(s: &str, f: Vec<f64>, p: &str, n: usize) {
    check_inference(
        "approx",
        &|p| {
            importance_weighting_h(
                n,
                p,
                &Options {
                    opt: USE_OPT,
                    debug: USE_DEBUG,
                    // seed: Some(9),
                    ..Default::default()
                },
            )
        },
        0.01,
        s,
        f,
        p,
    );
}
pub fn check_approx1(s: &str, f: f64, p: &str, n: usize) {
    check_approx(s, vec![f], p, n)
}

// pub fn debug_approx(s: &str, f: Vec<f64>, p: &str, n: usize) {
//     check_inference(
//         "debug",
//         &|p| {
//             importance_weighting_h(
//                 n,
//                 p,
//                 &Options {
//                     opt: USE_OPT,
//                     debug: USE_DEBUG,
//                     // seed: Some(9),
//                     ..Default::default()
//                 },
//             )
//         },
//         0.01,
//         s,
//         f,
//         p,
//     );
// }
// pub fn debug_approx1(s: &str, f: f64, p: &str, n: usize) {
//     debug_approx(s, vec![f], p, n)
// }
// pub fn nfail_approx(s: &str, f: Vec<f64>, p: &ProgramInferable, n: usize) {
//     check_inference_h(
//         "debug",
//         &|p| {
//             importance_weighting_h(
//                 n,
//                 p,
//                 &Options {
//                     opt: USE_OPT,
//                     debug: USE_DEBUG,
//                     // seed: Some(9),
//                     ..Default::default()
//                 },
//             )
//         },
//         0.01,
//         s,
//         f,
//         p,
//         false,
//     );
// }
// pub fn nfail_approx1(s: &str, f: f64, p: &ProgramInferable, n: usize) {
//     nfail_approx(s, vec![f], p, n)
// }
// // pub fn check_approx_conc(s: &str, f: Vec<f64>, p: &ProgramInferable, n: usize) {
// //     let env_args = EnvArgs::default_args(None);
// //     // let mut env = Env::from_args(&mut env_args);
// //     let precision = 0.01;
// //     let fs = f;
// //     let i = "approx";
// //     let prs = importance_weighting_conc(&env_args, n, p);
// //     assert_eq!(
// //         prs.len(),
// //         fs.len(),
// //         "check_inference compiled {} queries, tests expect {} results",
// //         prs.len(),
// //         fs.len(),
// //     );
// //     izip!(prs, fs).for_each(|(pr, f)| {
// //         let ret = (f - pr).abs() < precision;
// //         assert!(
// //             ret,
// //             "[check_{i}][{s}][err]((expected: {f}) - (actual: {pr})).abs < {precision}"
// //         );
// //     });
// // }
// // pub fn check_approx1_conc(s: &str, f: f64, p: &ProgramInferable, n: usize) {
// //     check_approx_conc(s, vec![f], p, n)
// // }

// pub fn check_approx_seeded(s: &str, f: Vec<f64>, p: &str, n: usize, seeds: &Vec<u64>) {
//     check_inference(
//         "approx",
//         &|env, p| importance_weighting_h(seeds.clone(), n, p),
//         0.01,
//         s,
//         f,
//         p,
//     );
// }
// pub fn check_approx_seeded1(s: &str, f: f64, p: &str, n: usize, seeds: &Vec<u64>) {
//     check_approx_seeded(s, vec![f], p, n, seeds)
// }
