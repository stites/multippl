use crate::annotate::{InvMap, LabelEnv};
use crate::compile::eval::State;
use crate::compile::grammar::ProgramTr;
use crate::compile::grammar::Trace;
use crate::data::HashMap;
use crate::typecheck::{
    grammar::{EExprTyped, ProgramTyped},
    typecheck,
};
use crate::typeinf::grammar::ProgramInferable;
use crate::typeinf::typeinference;
use crate::uniquify::{grammar::UniqueId, SymEnv};
use crate::*;
use itertools::*;
use rand::rngs::StdRng;
use tracing::debug;

#[derive(Debug, Clone, Default)]
pub struct Options {
    pub seed: Option<StdRng>,
    pub exact_only: bool,  // strip all sample statements from a program.
    pub debug: bool,       // overrides seed
    pub opt: bool,         // use optimizations
    pub stats_window: u64, // remove this?
}
impl Options {
    pub fn rng(&self) -> StdRng {
        match &self.seed {
            None => rand::SeedableRng::from_entropy(),
            Some(s) => s.clone(),
        }
    }
    pub fn stoch() -> Self {
        Default::default()
    }
    pub fn seed(s: u64) -> Self {
        Self {
            seed: Some(rand::SeedableRng::seed_from_u64(s)),
            ..Default::default()
        }
    }
    pub fn debug() -> Self {
        Self {
            debug: true,
            opt: false,
            ..Default::default()
        }
    }
    pub fn new(
        seed: Option<u64>,
        exact_only: bool,
        debug: bool,       // overrides seed
        opt: bool,         // use optimizations
        stats_window: u64, // use optimizations
    ) -> Self {
        Self {
            seed: seed.map(rand::SeedableRng::seed_from_u64),
            debug,
            opt,
            stats_window,
            exact_only,
        }
    }
}

// for now, only limited support for data points... Ideally this just turns into
// a for-loop in the language, but for now we can hack it as a PoC
#[derive(Clone, PartialEq, Debug)]
pub enum Datum {
    Float(f64), // will turn into SVals
    Int(u64),   // will turn into SVals
    Bool(bool), // will turn into EVals
    Bools(Vec<bool>),
    Floats(Vec<f64>),
    Ints(Vec<u64>),
}
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum DTy {
    SFloat, // will turn into SVals
    EBool,  // will turn into EVals
}

pub type DataPoints = HashMap<String, Vec<Datum>>;

#[derive(Clone, PartialEq, Debug)]
pub struct DataSet {
    ds: DataPoints,
    size: usize,
}
impl DataSet {
    pub fn empty() -> Self {
        DataSet {
            ds: Default::default(),
            size: 0,
        }
    }
    pub fn new(ds: DataPoints) -> Self {
        let mut size = 0;
        for col in ds.values() {
            if size == 0 {
                size = col.len();
            } else {
                assert_eq!(
                    size,
                    col.len(),
                    "data set has jagged lengths, please check your json file"
                );
            }
        }
        Self { ds, size }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct DataView1 {
    pub sampling: SubstMap<SVal>,
    pub exact: SubstMap<EVal>,
    pub keys: HashMap<String, UniqueId>,
}
#[derive(Clone, PartialEq, Debug)]
pub struct DataView {
    pub sampling: HashMap<String, Vec<SVal>>,
    pub exact: HashMap<String, Vec<EVal>>,
    pub size: usize,
    pub keys: Vec<(String, DTy, Option<UniqueId>)>,
}
impl DataView {
    pub fn empty() -> Self {
        Self::new(DataSet::empty())
    }
    pub fn new(data: DataSet) -> Self {
        let mut sampling = HashMap::default();
        let mut exact = HashMap::default();
        let mut keys = vec![];
        for (k, vs) in data.ds {
            if vs.is_empty() {
                break;
            } else {
                let is_sample = match vs[0] {
                    Datum::Float(_) => true,
                    Datum::Floats(_) => true,
                    Datum::Bool(_) => false,
                    Datum::Bools(_) => false,
                    Datum::Int(_) => true,
                    Datum::Ints(_) => true,
                };
                if is_sample {
                    let ss = vs
                        .into_iter()
                        .map(|v| match v {
                            Datum::Float(f) => SVal::SFloat(f),
                            Datum::Int(f) => SVal::SInt(f),
                            Datum::Floats(fs) => {
                                SVal::SVec(fs.into_iter().map(SVal::SFloat).collect_vec())
                            }
                            Datum::Ints(fs) => {
                                SVal::SVec(fs.into_iter().map(SVal::SInt).collect_vec())
                            }
                            Datum::Bool(_) => panic!(""),
                            Datum::Bools(_) => panic!(""),
                        })
                        .collect_vec();
                    sampling.insert(k.clone(), ss);
                    keys.push((k, DTy::SFloat, None));
                } else {
                    let bs = vs
                        .into_iter()
                        .map(|v| match v {
                            Datum::Bool(b) => EVal::from_bool(b),
                            Datum::Bools(bs) => {
                                EVal::EProd(bs.into_iter().map(EVal::from_bool).collect_vec())
                            }
                            Datum::Float(f) => panic!(""),
                            Datum::Int(f) => panic!(""),
                            Datum::Floats(fs) => panic!(""),
                            Datum::Ints(fs) => panic!(""),
                        })
                        .collect_vec();
                    exact.insert(k.clone(), bs);
                    keys.push((k, DTy::EBool, None));
                }
            }
        }
        Self {
            sampling,
            exact,
            size: data.size,
            keys,
        }
    }
    pub fn view(&self, step: usize) -> DataView1 {
        if self.size == 0 {
            DataView1 {
                sampling: Default::default(),
                exact: Default::default(),
                keys: Default::default(),
            }
        } else {
            let ix = step % self.size;
            let keys: HashMap<String, UniqueId> = self
                .keys
                .iter()
                .map(|(s, ty, id)| (s.clone(), id.unwrap()))
                .collect();
            let sampling: HashMap<UniqueId, SVal> = self
                .sampling
                .iter()
                .map(|(k, vs)| (*keys.get(k).unwrap(), vs[ix].clone()))
                .collect();
            let exact: HashMap<UniqueId, EVal> = self
                .exact
                .iter()
                .map(|(k, vs)| (*keys.get(k).unwrap(), vs[ix].clone()))
                .collect();
            DataView1 {
                sampling,
                exact,
                keys,
            }
        }
    }
}

pub struct ROut {
    pub out: Output,
    pub rng: Option<StdRng>,
    pub mgr: Mgr,
    pub weight: Ln,
    pub wmcp: WmcP,
}

pub struct PartialROut {
    pub out: Output,
    pub weight: Ln,
    pub wmcp: WmcP,
    pub rng: Option<StdRng>,
}
impl PartialROut {
    pub fn to_rout(p: Self, mgr: Mgr) -> ROut {
        let rng = p.rng.clone();
        let out = p.out.clone();
        let weight = p.weight;
        let wmcp = p.wmcp;

        ROut {
            mgr,
            out,
            rng,
            weight,
            wmcp,
        }
    }
}
#[macro_export]
macro_rules! run {
    ($code:expr) => {{
        let opt = $crate::pipeline::Options::stoch();
        run!($code, opt)
    }};
    ($code:expr; --split exact) => {{
        let mut opt = $crate::pipeline::Options::stoch();
        opt.exact_only = true;
        run!($code, opt)
    }};
    ($code:expr; --erased) => {{
        let mut opt = $crate::pipeline::Options::stoch();
        opt.exact_only = true;
        let exact = $code.strip_samples().ok().unwrap();
        let (mut mgr, p, lenv) = make_mgr_and_ir(&exact).ok().unwrap();
        tracing::debug!(",====================================.");
        tracing::debug!("| manager compiled! building program |");
        tracing::debug!("`===================================='");
        let r = runner(&mut mgr, &mut opt.rng(), &opt, &p, &lenv)
            .ok()
            .unwrap();
        r.to_rout(mgr)
    }};
    ($code:expr, $opt:expr) => {{
        match $crate::pipeline::run($code, &$opt) {
            Ok(o) => o,
            Err(e) => panic!(
                "\nCompiler Error!!!\n==============\n{}\n==============\n",
                e
            ),
        }
    }};
}

pub fn run(code: &str, opt: &Options) -> Result<ROut> {
    let (mut mgr, p, lenv) = make_mgr_and_ir(code)?;
    let wmc = WmcP::new_with_size(mgr.get_order().num_vars());
    tracing::debug!(",====================================.");
    tracing::debug!("| manager compiled! building program |");
    tracing::debug!("`===================================='");
    let r = runner(&mut mgr, &mut opt.rng(), wmc, opt, &p, &lenv)?;
    Ok(PartialROut::to_rout(r, mgr))
}

pub fn runner(
    mgr: &mut Mgr,
    rng: &mut StdRng,
    wmc: WmcP,
    opt: &Options,
    p: &Program<crate::annotate::grammar::Annotated>,
    lenv: &LabelEnv,
) -> Result<PartialROut> {
    let sample_pruning = opt.opt;
    let step = 0;
    let dv = &DataView::empty();

    tracing::debug!("program running...");
    let mut state = State::new(
        mgr,
        wmc,
        Some(rng),
        sample_pruning,
        &lenv.funs,
        &lenv.fun_stats,
    );
    let out = state.eval_program_with_data(p, &dv.view(step))?;
    tracing::debug!("program... compiled!");
    let weight = state.log_weight();
    let rng = state.rng.cloned();
    let wmcp = state.wmc;
    // println!("{}", state.mgr.get_order().num_vars());

    Ok(PartialROut {
        out,
        wmcp,
        weight,
        rng,
    })
}

pub fn make_mgr_and_ir(
    code: &str,
) -> Result<(Mgr, Program<crate::annotate::grammar::Annotated>, LabelEnv)> {
    tracing::info!("compiling code:\n{code}");
    tracing::trace!("making manager");
    let p = crate::parser::program::parse(code)?;
    make_mgr_and_ir_h(&p)
}

pub fn make_mgr_and_ir_h(
    p: &ProgramInferable,
) -> Result<(Mgr, Program<crate::annotate::grammar::Annotated>, LabelEnv)> {
    tracing::debug!("(parsed)");
    tracing::debug!("(parsed) >>> {p:?}");
    tracing::debug!("(parsed)");
    let p = crate::typeinf::typeinference(p)?;
    tracing::debug!("(inferred)");
    tracing::debug!("(inferred) >>> {p:?}");
    tracing::debug!("(inferred)");
    let p = crate::typecheck::typecheck(&p)?;
    tracing::debug!("(checked)");
    tracing::debug!("(checked) >>> {p:?}");
    tracing::debug!("(checked)");
    let p = crate::desugar::SugarMagicEnv::new().desugar(&p)?;
    tracing::debug!("(desugared)");
    tracing::debug!("(desugared) >>> {p:?}");
    tracing::debug!("(desugared)");
    let mut env = SymEnv::default();

    let mut dv = DataView::new(DataSet::empty());
    let p = env.uniquify_with_data(&p, &mut dv)?.0;
    tracing::debug!("(uniquifyed)");
    tracing::debug!("(uniquifyed) >>> {p:?}");
    tracing::debug!("(uniquifyed)");
    let mut lenv = LabelEnv::new(env.functions, env.fun_stats);
    let ar = lenv.annotate_with_data(&p, &dv)?;
    let p = ar.program;
    tracing::debug!("(annotated)");
    tracing::debug!("(annotated) >>> {p:?}");
    tracing::debug!("(annotated)");
    let maxlbl = ar.maxbdd.0;
    tracing::debug!("(manager created with max label: {maxlbl})");
    let mgr = crate::data::new_manager(maxlbl);
    Ok((mgr, p, lenv))
}
