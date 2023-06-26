use crate::utils::render::*;
use crate::PQ;
use itertools::*;
use std::iter::Sum;

#[derive(Debug, Clone)]
pub struct Expectations {
    pub exp: Vec<f64>,
    pub expw: Vec<f64>,
    pub expw2: Vec<f64>,
    pub cached_query: Option<Vec<f64>>,
    pub cached_variance: Option<Vec<f64>>,
}
impl Expectations {
    pub fn empty() -> Self {
        Self {
            exp: vec![],
            expw: vec![],
            expw2: vec![],
            cached_query: None,
            cached_variance: None,
        }
    }
    pub fn new(pq: PQ, prs: Vec<f64>) -> Self {
        let (exp, expw, expw2) = prs.into_iter().fold(
            (vec![], vec![], vec![]),
            |(mut exp, mut expw, mut expw2), pr| {
                let w = pq.weight();
                exp.push(w * pr);
                expw.push(w);
                expw2.push(w * w);
                (exp, expw, expw2)
            },
        );
        Self {
            exp,
            expw,
            expw2,
            cached_query: None,
            cached_variance: None,
        }
    }
    pub fn add(l: Self, o: Self) -> Self {
        let mut x = l.clone();
        x.mut_add(&o);
        x
    }

    pub fn mut_add(&mut self, o: &Self) {
        if self.exp.is_empty() {
            self.exp = o.exp.clone();
            self.expw = o.expw.clone();
            self.expw2 = o.expw2.clone();
            self.cached_query = o.cached_query.clone();
            self.cached_variance = o.cached_variance.clone();
        } else {
            self.exp = izip!(&self.exp, &o.exp).map(|(l, r)| l + r).collect_vec();
            self.expw = izip!(&self.expw, &o.expw).map(|(l, r)| l + r).collect_vec();
            self.cached_query = None;
            self.cached_variance = None;
        }
    }

    pub fn to_str(&self) -> String {
        let mut s = String::new();
        s.push_str(&String::from("exp : "));
        s.push_str(&renderfloats(&self.exp, false));
        s.push('\n');
        s.push_str(&String::from("expw: "));
        s.push_str(&renderfloats(&self.expw, false));
        s.push('\n');
        s
    }
    pub fn query(&self) -> Vec<f64> {
        izip!(&self.exp, &self.expw)
            .map(|(exp, expw)| if exp == &0.0 { 0.0 } else { exp / expw })
            .collect_vec()
    }
    pub fn compute_query(&mut self) -> Vec<f64> {
        match &self.cached_query {
            None => {
                let qs = self.query();
                self.cached_query = Some(qs.clone());
                qs
            }
            Some(q) => q.clone(),
        }
    }
    pub fn var(&self) -> Vec<f64> {
        izip!(&self.expw2, &self.expw)
            .map(|(exp, expw)| if exp == &0.0 { 0.0 } else { exp / expw })
            .collect_vec()
    }
    pub fn compute_var(&mut self) -> Vec<f64> {
        match &self.cached_variance {
            None => {
                let qs = self.var();
                self.cached_variance = Some(qs.clone());
                qs
            }
            Some(q) => q.clone(),
        }
    }
}

impl Sum for Expectations {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Expectations>,
    {
        iter.fold(Expectations::empty(), Expectations::add)
    }
}
