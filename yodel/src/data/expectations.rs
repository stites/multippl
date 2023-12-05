use crate::utils::render::*;
use crate::{LPQ, LW};
use itertools::*;
use std::iter::Sum;

pub fn log_space_add(lx: f64, ly: f64) -> f64 {
    if !ly.is_infinite() {
        lx + (ly - lx).exp().ln_1p()
    } else {
        lx
    }
}
#[derive(Debug, Clone)]
pub struct Exp1 {
    pub wquery_sums: Vec<f64>, // queries *already* multiplied by the log-weight
    pub lwquery_sums: Vec<f64>, // queries *already* multiplied by the log-weight
    pub query_sums: Vec<f64>,  // queries (for debugging)
    pub sum_w: f64,            // sum of weights
    pub sum_lw: f64,           // sum of log-weights
    pub sum_w2: f64,           // sum of squared weights
    pub count: u64,            // number of samples drawn
}
impl Exp1 {
    pub fn empty() -> Self {
        Self {
            query_sums: vec![],
            wquery_sums: vec![],
            lwquery_sums: vec![],
            sum_w: 0.0,
            sum_lw: 0.0,
            sum_w2: 0.0,
            count: 0,
        }
    }
    pub fn new(lw: LW, qs: Vec<f64>) -> Self {
        let w = lw.exp();
        Self {
            query_sums: qs.iter().copied().collect_vec(),
            wquery_sums: qs.iter().map(|q| w * q).collect_vec(),
            // lwquery_sums: qs.iter().map(|q| lw.add(*q).val()).collect_vec(),
            lwquery_sums: vec![],
            sum_w: w,
            sum_lw: lw.val(),
            sum_w2: w * w,
            count: 1,
        }
    }
    pub fn add(&mut self, o: Self) {
        if self.count == 0 {
            self.query_sums = o.query_sums;
            self.wquery_sums = o.wquery_sums;
            self.lwquery_sums = o.lwquery_sums;
            self.sum_w = o.sum_w;
            self.sum_lw = o.sum_lw;
            self.sum_w2 = o.sum_w2;
            self.count = o.count;
        } else {
            if !o.sum_w.is_nan() {
                self.sum_w += o.sum_w;
                self.sum_lw = log_space_add(self.sum_lw, o.sum_lw);

                self.sum_w2 += o.sum_w2;
                self.count += o.count;

                self.query_sums = izip!(&self.query_sums, &o.query_sums)
                    .map(|(l, r)| l + r)
                    .collect_vec();
                self.wquery_sums = izip!(&self.wquery_sums, &o.wquery_sums)
                    .map(|(l, r)| l + r)
                    .collect_vec();
                self.lwquery_sums = izip!(&self.lwquery_sums, &o.lwquery_sums)
                    .map(|(l, r)| log_space_add(*l, *r))
                    .collect_vec();
            }
        }
    }
    pub fn query(&self) -> Vec<f64> {
        let x = self
            .wquery_sums
            .iter()
            .map(|q| if q == &0.0 { 0.0 } else { q / self.sum_w })
            .collect_vec();

        // let y = self
        //     .lwquery_sums
        //     .iter()
        //     .map(|q| (q - self.sum_lw).exp())
        //     .collect_vec();
        x
    }
    pub fn var(&self) -> f64 {
        if self.sum_w2 == 0.0 {
            0.0
        } else {
            self.sum_w2 / self.sum_w
        }
    }
}

// #[derive(Debug, Clone)]
// pub struct Exp2 {
//     pub wquery_sums: Vec<f64>, // queries *already* multiplied by the weight
//     pub sum_w: Vec<f64>,       // sum of weights
//     pub sum_w2: Vec<f64>,      // sum of squared weights
//     pub count: u64,            // number of samples drawn
// }
// impl Exp2 {
//     pub fn empty() -> Self {
//         Self {
//             wquery_sums: vec![],
//             sum_w: vec![],
//             sum_w2: vec![],
//             count: 0,
//         }
//     }
//     pub fn new(w: Vec<f64>, qs: Vec<f64>) -> Self {
//         todo!()
//         // Self {
//         //     wquery_sums: qs.into_iter().map(|q| w * q).collect_vec(),
//         //     sum_w: w.clone(),
//         //     sum_w2: w.into_iter().map(|w| w * w).collect_vec(),
//         //     count: 1,
//         // }
//     }
//     pub fn add(&mut self, o: Self) {
//         if self.count == 0 {
//             self.wquery_sums = o.wquery_sums;
//             self.sum_w = o.sum_w;
//             self.sum_w2 = o.sum_w2;
//             self.count = o.count;
//         } else {
//             self.wquery_sums = izip!(&self.wquery_sums, &o.wquery_sums)
//                 .map(|(l, r)| l + r)
//                 .collect_vec();
//             self.sum_w = izip!(&self.sum_w, &o.sum_w)
//                 .map(|(l, r)| l + r)
//                 .collect_vec();
//             self.sum_w2 = izip!(&self.sum_w2, &o.sum_w2)
//                 .map(|(l, r)| l + r)
//                 .collect_vec();
//             self.count += o.count;
//         }
//     }
//     pub fn query(&self) -> Vec<f64> {
//         izip!(&self.wquery_sums, &self.sum_w)
//             .map(|(q, w)| if q == &0.0 { 0.0 } else { q / w })
//             .collect_vec()
//     }
//     pub fn var(&self) -> Vec<f64> {
//         izip!(&self.sum_w2, &self.sum_w)
//             .map(|(w2, w)| if w == &0.0 { 0.0 } else { w2 / w })
//             .collect_vec()
//     }
// }

// #[derive(Debug, Clone)]
// pub struct Expectations {
//     pub exp: Vec<f64>,
//     pub expw: Vec<f64>,
//     pub expw2: Vec<f64>,
//     pub cached_query: Option<Vec<f64>>,
//     pub cached_variance: Option<Vec<f64>>,
// }
// impl Expectations {
//     pub fn empty() -> Self {
//         Self {
//             exp: vec![],
//             expw: vec![],
//             expw2: vec![],
//             cached_query: None,
//             cached_variance: None,
//         }
//     }
//     pub fn new(pq: LPQ, prs: Vec<f64>) -> Self {
//         let (exp, expw, expw2) = prs.into_iter().fold(
//             (vec![], vec![], vec![]),
//             |(mut exp, mut expw, mut expw2), pr| {
//                 let w = pq.weight();
//                 exp.push(w * pr);
//                 expw.push(w);
//                 expw2.push(w * w);
//                 (exp, expw, expw2)
//             },
//         );
//         Self {
//             exp,
//             expw,
//             expw2,
//             cached_query: None,
//             cached_variance: None,
//         }
//     }
//     pub fn add(l: Self, o: Self) -> Self {
//         let mut x = l.clone();
//         x.mut_add(&o);
//         x
//     }

//     pub fn mut_add(&mut self, o: &Self) {
//         if self.exp.is_empty() {
//             self.exp = o.exp.clone();
//             self.expw = o.expw.clone();
//             self.expw2 = o.expw2.clone();
//             self.cached_query = o.cached_query.clone();
//             self.cached_variance = o.cached_variance.clone();
//         } else {
//             self.exp = izip!(&self.exp, &o.exp).map(|(l, r)| l + r).collect_vec();
//             self.expw = izip!(&self.expw, &o.expw).map(|(l, r)| l + r).collect_vec();
//             self.cached_query = None;
//             self.cached_variance = None;
//         }
//     }

//     pub fn to_str(&self) -> String {
//         let mut s = String::new();
//         s.push_str(&String::from("exp : "));
//         s.push_str(&renderfloats(&self.exp, false));
//         s.push('\n');
//         s.push_str(&String::from("expw: "));
//         s.push_str(&renderfloats(&self.expw, false));
//         s.push('\n');
//         s
//     }
//     pub fn query(&self) -> Vec<f64> {
//         izip!(&self.exp, &self.expw)
//             .map(|(exp, expw)| if exp == &0.0 { 0.0 } else { exp / expw })
//             .collect_vec()
//     }
//     pub fn compute_query(&mut self) -> Vec<f64> {
//         match &self.cached_query {
//             None => {
//                 let qs = self.query();
//                 self.cached_query = Some(qs.clone());
//                 qs
//             }
//             Some(q) => q.clone(),
//         }
//     }
//     pub fn var(&self) -> Vec<f64> {
//         izip!(&self.expw2, &self.expw)
//             .map(|(exp, expw)| if exp == &0.0 { 0.0 } else { exp / expw })
//             .collect_vec()
//     }
//     pub fn compute_var(&mut self) -> Vec<f64> {
//         match &self.cached_variance {
//             None => {
//                 let qs = self.var();
//                 self.cached_variance = Some(qs.clone());
//                 qs
//             }
//             Some(q) => q.clone(),
//         }
//     }
// }

// impl Sum for Expectations {
//     fn sum<I>(iter: I) -> Self
//     where
//         I: Iterator<Item = Expectations>,
//     {
//         iter.fold(Expectations::empty(), Expectations::add)
//     }
// }
