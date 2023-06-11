pub mod anf;
pub mod exact;
pub mod grammar;

use crate::annotate::grammar::BddVar;
use crate::annotate::grammar::ProgramAnn;
use either::Either;
use either::*;
use grammar::*;

use crate::annotate::grammar::NamedVar;
use crate::data::*;
use rand::rngs::StdRng;
use rsdd::repr::wmc::RealSemiring;
use std::collections::HashMap;
use std::collections::HashSet;
use tracing::*;

use crate::annotate::grammar::Var;
use crate::uniquify::grammar::UniqueId;
use rsdd::repr::var_order::VarOrder;

use rsdd::repr::wmc::WmcParams;

pub struct Env<'a> {
    pub mgr: &'a mut Mgr,
    pub rng: Option<&'a mut StdRng>, // None implies "debug mode"

    // pre-computed
    pub order: VarOrder,
    pub max_label: u64,

    // in progress
    pub sample_pruning: bool,
    pub inv: HashMap<NamedVar, HashSet<BddVar>>,

    // static
    pub varmap: Option<HashMap<UniqueId, Var>>,

    // ignored
    pub weightmap: Option<WmcParams<RealSemiring>>,
}

impl<'a> Env<'a> {
    pub fn new(
        mgr: &'a mut Mgr,
        rng: Option<&'a mut StdRng>,
        sample_pruning: bool,
        inv: HashMap<NamedVar, HashSet<BddVar>>,
    ) -> Env<'a> {
        Env {
            order: mgr.get_order().clone(),
            weightmap: None,
            varmap: None,
            max_label: mgr.get_order().num_vars() as u64,
            inv,
            mgr,
            rng,
            sample_pruning,
        }
    }
}

pub fn debug(env: &mut Env, p: &ProgramAnn) -> Result<(Compiled, Either<EExprTr, SExprTr>)> {
    use crate::grammar::Program;
    debug!("========================================================");
    match p {
        Program::SBody(e) => {
            // let (c, e) = env.eval_sexpr(&Default::default(), e)?;
            let (c, e) = todo!();
            Ok((c, Right(e)))
        }
        Program::EBody(e) => {
            // let (c, e) = env.eval_eexpr(&Default::default(), e)?;
            let (c, e) = todo!();
            Ok((c, Left(e)))
        }
    }
}

pub fn compile(env: &mut Env, p: &ProgramAnn) -> Result<Compiled> {
    Ok(debug(env, p)?.0)
}
