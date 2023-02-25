use itertools::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Importance {
    Weight(f64),
    // Sample(bool, f64),        // an importance sample always knows its weight
    Worlds(Vec<(bool, f64)>), // different samples which denote parallel execution traces
}
pub type I = Importance;
impl num_traits::identities::One for Importance {
    fn one() -> Self {
        I::Weight(1.0)
    }
}
impl num_traits::identities::Zero for Importance {
    fn zero() -> Self {
        I::Weight(0.0)
    }
    fn is_zero(&self) -> bool {
        match self {
            I::Weight(w) => *w == 0.0,
            _ => false,
        }
    }
}
impl core::ops::Mul for Importance {
    type Output = Importance;
    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (I::Weight(l), I::Weight(r)) => I::Weight(l * r),
            _ => panic!("no."),
        }
    }
}
impl core::ops::Add for Importance {
    type Output = Importance;
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (I::Weight(l), I::Weight(r)) => I::Weight(l + r),
            _ => panic!("no."),
        }
    }
}
impl IntoIterator for Importance {
    type Item = f64;
    type IntoIter = std::vec::IntoIter<f64>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Importance::Weight(w) => vec![w].into_iter(),
            Importance::Worlds(ws) => ws.iter().map(|(s, w)| w).cloned().collect_vec().into_iter(),
        }
    }
}

impl Importance {
    #[inline]
    pub fn weight(&self) -> f64 {
        self.weight_safe()
            .unwrap_or_else(|| panic!("importance weight is fully enumerated"))
    }
    #[inline]
    pub fn weight_safe(&self) -> Option<f64> {
        match *self {
            Importance::Weight(w) => Some(w),
            Importance::Worlds(_) => None,
        }
    }
}
