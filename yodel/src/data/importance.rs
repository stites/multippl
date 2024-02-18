use itertools::*;
use rsdd::sample::probability::Probability;

// #[derive(Copy, Clone, Debug, PartialEq)]
// pub struct PQ {
//     pub p: f64,
//     pub q: f64,
// }
// impl PQ {
//     pub fn weight(&self) -> f64 {
//         self.p / self.q
//     }
//     pub fn render(&self) -> String {
//         format!("{:.6} / {:.6}", self.p, self.q)
//     }
//     pub fn ln(&self) -> LPQ {
//         LPQ {
//             lp: LW::new(self.p),
//             lq: LW::new(self.q),
//         }
//     }
// }
// impl Default for PQ {
//     fn default() -> Self {
//         PQ { p: 1.0, q: 1.0 }
//     }
// }

#[derive(PartialEq, Copy, Clone, Debug)]
pub struct LW(pub f64);
impl LW {
    pub fn val(&self) -> f64 {
        self.0
    }
    pub fn exp(&self) -> f64 {
        self.val().exp()
    }
    pub fn new(p: f64) -> Self {
        Self(p.ln())
    }
    pub fn add(&self, o: Self) -> Self {
        Self(self.0 + o.0)
    }
    pub fn sub(&self, o: Self) -> Self {
        Self(self.0 - o.0)
    }
    pub fn render(&self) -> String {
        format!("LW(weight={:.6})", self.exp())
    }
    pub fn log_render(&self) -> String {
        format!("LW(log_weight={:.6})", self.val())
    }
}
impl Default for LW {
    fn default() -> Self {
        Self(0.0)
    }
}

// #[derive(Copy, Clone, Debug, PartialEq, Default)]
// pub struct LPQ {
//     pub lp: LW,
//     pub lq: LW,
// }
// impl LPQ {
//     pub fn log_weight(&self) -> LW {
//         self.lp.sub(self.lq)
//     }
//     pub fn render(&self) -> String {
//         format!("LPQ(p={}, q={})", self.lp.render(), self.lq.render())
//     }
//     pub fn log_render(&self) -> String {
//         format!(
//             "LPQ(p={}, q={})",
//             self.lp.log_render(),
//             self.lq.log_render()
//         )
//     }
//     pub fn exp(&self) -> PQ {
//         PQ {
//             p: self.lp.exp(),
//             q: self.lq.exp(),
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum Importance {
//     Weight(f64),
//     // Sample(bool, f64),        // an importance sample always knows its weight
// }
// pub type I = Importance;
// impl num_traits::identities::One for Importance {
//     fn one() -> Self {
//         I::Weight(1.0)
//     }
// }
// impl num_traits::identities::Zero for Importance {
//     fn zero() -> Self {
//         I::Weight(0.0)
//     }
//     fn is_zero(&self) -> bool {
//         match self {
//             I::Weight(w) => *w == 0.0,
//         }
//     }
// }
// impl core::ops::Mul for Importance {
//     type Output = Importance;
//     fn mul(self, rhs: Self) -> Self {
//         match (self, rhs) {
//             (I::Weight(l), I::Weight(r)) => I::Weight(l * r),
//         }
//     }
// }
// impl core::ops::Add for Importance {
//     type Output = Importance;
//     fn add(self, rhs: Self) -> Self {
//         match (self, rhs) {
//             (I::Weight(l), I::Weight(r)) => I::Weight(l + r),
//         }
//     }
// }
// impl IntoIterator for Importance {
//     type Item = f64;
//     type IntoIter = std::vec::IntoIter<f64>;

//     fn into_iter(self) -> Self::IntoIter {
//         match self {
//             Importance::Weight(w) => vec![w].into_iter(),
//         }
//     }
// }
// impl Importance {
//     #[inline]
//     pub fn weight(&self) -> f64 {
//         self.weight_safe()
//             .unwrap_or_else(|| panic!("importance weight is fully enumerated"))
//     }
//     #[inline]
//     pub fn weight_safe(&self) -> Option<f64> {
//         match *self {
//             Importance::Weight(w) => Some(w),
//         }
//     }
//     #[inline]
//     pub fn pr_mul(&self, p: Probability) -> Importance {
//         match *self {
//             Importance::Weight(w) => Importance::Weight(w * p.as_f64()),
//         }
//     }
// }
