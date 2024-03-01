pub mod context;
pub mod errors;
pub mod expectations;
pub mod importance;
pub mod output;
pub mod weight;
pub mod wmcstats;
pub use crate::data::context::*;
pub use crate::data::errors::CompileError::*;
pub use crate::data::errors::*;
pub use crate::data::expectations::*;
pub use crate::data::importance::*;
pub use crate::data::output::*;
pub use crate::data::weight::*;
pub use crate::data::wmcstats::*;

// reexports and aliases
pub use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
pub use rsdd::builder::cache::lru_app::BddApplyTable;
pub use rsdd::repr::bdd::BddPtr;
use rsdd::repr::var_label::VarLabel;
use rsdd::repr::wmc::{RealSemiring, WmcParams};
pub use rsdd::sample::probability::Probability;
pub use rustc_hash::{FxHashMap, FxHashSet};

// pub type Mgr = BddManager<AllTable<BddPtr>>;
pub type Mgr = BddManager<BddApplyTable<BddPtr>>;
pub type HashMap<K, V> = FxHashMap<K, V>;
pub type HashSet<V> = FxHashSet<V>;

#[derive(Debug, Clone)]
pub struct WmcP(WmcParams<RealSemiring>);

impl WmcP {
    pub fn params(&self) -> &WmcParams<RealSemiring> {
        &self.0
    }
    pub fn insert_high(&mut self, lbl: VarLabel, param: f64) {
        self.0
            .set_weight(lbl, RealSemiring(1.0 - param), RealSemiring(param))
    }
    pub fn new() -> Self {
        WmcP(WmcParams::new(RealSemiring(0.0), RealSemiring(1.0)))
    }
    pub fn new_with_size(sz: usize) -> Self {
        WmcP(WmcParams::new_with_size(
            RealSemiring(0.0),
            RealSemiring(1.0),
            sz,
        ))
    }
}
