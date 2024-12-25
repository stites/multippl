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
use num_traits::Float;

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
pub fn new_manager(maxlbl: u64) -> Mgr {
    // Mgr::new_default_order(maxlbl as usize)
    // Mgr::new_default_order(0_usize);
    Mgr::new_default_order_lru(0_usize)
}
pub type HashMap<K, V> = FxHashMap<K, V>;
pub type HashSet<V> = FxHashSet<V>;

#[derive(Debug)]
pub struct WmcP(pub WmcParams<RealSemiring>);

impl Default for WmcP {
    fn default() -> Self {
        WmcP(WmcParams::new(RealSemiring(0.0), RealSemiring(1.0)))
    }
}
impl WmcP {
    pub fn params(&self) -> &WmcParams<RealSemiring> {
        &self.0
    }
    pub fn insert_high(&mut self, lbl: VarLabel, param: f64) {
        let lo = 1.0 - param;
        let hi = param;
        self.0.set_weight(lbl, RealSemiring(lo), RealSemiring(hi))
    }
    pub fn new_with_size(sz: usize) -> Self {
        WmcP(WmcParams::new_with_size(
            RealSemiring(0.0),
            RealSemiring(1.0),
            sz,
        ))
    }
}
