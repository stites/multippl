use itertools::*;
use rsdd::sample::probability::Probability;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct PQ {
    pub p: f64,
    pub q: f64,
}
impl PQ {
    pub fn weight(&self) -> f64 {
        self.p / self.q
    }
    pub fn render(&self) -> String {
        format!("{:.6} / {:.6}", self.p, self.q)
    }
    pub fn ln(&self) -> LPQ {
        LPQ {
            lp: self.p.ln(),
            lq: self.q.ln(),
        }
    }
}
impl Default for PQ {
    fn default() -> Self {
        PQ { p: 1.0, q: 1.0 }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct LPQ {
    pub lp: f64,
    pub lq: f64,
}
impl LPQ {
    pub fn weight(&self) -> f64 {
        self.log_weight().exp()
    }
    pub fn log_weight(&self) -> f64 {
        self.lp - self.lq
    }
    pub fn render(&self) -> String {
        format!("{:.6} / {:.6}", self.lp.exp(), self.lq.exp())
    }
    pub fn log_render(&self) -> String {
        format!("{:.3} - {:.3}", self.lp, self.lq)
    }
    pub fn exp(&self) -> PQ {
        PQ {
            p: self.lp.exp(),
            q: self.lq.exp(),
        }
    }
}
impl Default for LPQ {
    fn default() -> Self {
        LPQ { lp: 0.0, lq: 0.0 }
    }
}

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
