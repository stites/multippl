use rsdd::repr::bdd::VarLabel;
use rsdd::repr::wmc::{RealSemiring, WmcParams};
use crate::data::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Weight {
    pub lo: RealSemiring,
    pub hi: RealSemiring,
}
impl Weight {
    pub fn new(lo: f64, hi: f64) -> Weight {
        Weight {
            lo: RealSemiring(lo),
            hi: RealSemiring(hi),
        }
    }
    pub fn as_tuple(&self) -> (RealSemiring, RealSemiring) {
        (self.lo, self.hi)
    }
    pub fn from_high(hi: f64) -> Weight {
        Weight {
            lo: RealSemiring(1.0 - hi),
            hi: RealSemiring(hi),
        }
    }
    pub fn constant() -> Weight {
        Weight {
            lo: RealSemiring(1.0),
            hi: RealSemiring(1.0),
        }
        // Self::from_high(0.5)
    }
}
