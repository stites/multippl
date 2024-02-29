use crate::data::{Weight, HashMap};
use rsdd::repr::bdd::VarLabel;
use rsdd::repr::wmc::{RealSemiring, WmcParams};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct WeightMap {
    pub weights: HashMap<VarLabel, Weight>,
}

impl WeightMap {
    pub fn as_params(&self, max_label: u64) -> WmcParams<RealSemiring> {
        let mut wmc_params = WmcParams::new(RealSemiring(0.0), RealSemiring(1.0));
        let cnst = Weight::constant();
        for ix in 0..max_label {
            let lbl = VarLabel::new(ix);
            let weight = self.weights.get(&lbl).unwrap_or(&cnst);
            wmc_params.set_weight(lbl, weight.lo, weight.hi);
        }
        for (label, weight) in self.weights.iter() {
            wmc_params.set_weight(*label, weight.lo, weight.hi);
        }
        wmc_params
    }
    pub fn insert(&mut self, lbl: VarLabel, high: f64) {
        self.weights.insert(lbl, Weight::from_high(high));
    }
}

impl IntoIterator for WeightMap {
    type Item = (VarLabel, Weight);
    type IntoIter = std::collections::hash_map::IntoIter<VarLabel, Weight>;

    fn into_iter(self) -> Self::IntoIter {
        self.weights.into_iter()
    }
}
