// use crate::typecheck::grammar::{ExprTyped, ProgramTyped};
// use tracing::*;

// pub fn debug(p: &ProgramTyped) -> Result<Compiled, CompileError> {
//     use crate::annotate::LabelEnv;
//     use crate::typecheck::typecheck;
//     use crate::uniquify::SymEnv;

//     let p = typecheck(p)?;
//     let mut senv = SymEnv::default();
//     let p = senv.uniquify(&p)?;
//     let mut lenv = LabelEnv::new();
//     let (p, vo, varmap, inv, mxlbl) = lenv.annotate(&p)?;

//     let mut env_args = EnvArgs::default_args(None);
//     let mut env = Env::from_args(&mut env_args);
//     env.names = senv.names; // just for debugging, really.
//     env.order = Some(vo);
//     env.max_label = Some(mxlbl);
//     env.varmap = Some(varmap);
//     env.inv = Some(inv);
//     let c = compile(&mut env, &p);
//     tracing::debug!("hurray!");
//     c
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::compile::*;
//     use crate::grammar::*;
//     use crate::grammar_macros::*;
//     use rsdd::builder::bdd_builder::*;
//     use rsdd::builder::cache::all_app::*;

//     use rsdd::repr::bdd::*;
//     use rsdd::repr::ddnnf::DDNNFPtr;
//     use rsdd::repr::var_label::*;
//     use rsdd::*;
//     use tracing_test::traced_test;

//     #[test]
//     #[traced_test]
//     fn enumerate_samples() {
//         let mk = |ret: ExprTyped| {
//             Program::Body(lets![
//                 "x" ; b!() ;= flip!(1/5);
//                 // "y" ; b!() ;= ite!(
//                 //     if ( var!("x") )
//                 //     then { sample!(flip!(1/3)) }
//                 //     else { flip!(1/4) });
//                 ...? ret ; b!()
//             ])
//         };
//         let p = mk(b!("y"));

//         match debug(&p) {
//             Ok(cs) => {
//                 debug!("{:?}", cs);
//             }
//             Err(e) => panic!("{:?}", e),
//         }
//     }
// }
