use crate::inference::wmc::*;
use crate::*;
use itertools::*;
use rand::rngs::StdRng;
use std::time::Duration;
use std::time::Instant;

pub struct SamplingIter {
    pub current_step: usize,
    pub current_exp: Expectations,
    pub max_steps: usize,
    pub opt: crate::Options, // can only be 1
    pub rng: StdRng,
    pub start: Instant,
    pub code: String,
    pub manager: Mgr,
    pub max_stats: WmcStats,
}
impl SamplingIter {
    pub fn new(steps: usize, code: &str, opt: &crate::Options) -> Self {
        let mgr = crate::make_mgr(code).unwrap();
        let rng = opt.rng();
        Self {
            current_step: 0,
            current_exp: Expectations::empty(),
            max_steps: steps,
            opt: opt.clone(),
            start: Instant::now(),
            rng: rng,
            code: code.to_string(),
            manager: mgr,
            max_stats: WmcStats::empty(),
        }
    }
}
pub struct SamplingResult {
    pub step: usize,
    pub stats: WmcStats,
    pub expectations: Expectations,
    pub duration: Duration,
    pub weight: PQ,
}

impl Iterator for SamplingIter {
    type Item = SamplingResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_step >= self.max_steps {
            return None;
        }
        match crate::runner(&self.code, &mut self.manager, &mut self.rng, &self.opt) {
            Ok(r) => {
                let (c, rng, pq) = (r.out, r.rng, r.pq);
                let mut newopt = self.opt.clone();
                newopt.seed = rng;
                self.opt = newopt;
                let step = self.current_step;

                self.current_step += 1;
                let (query_result, stats) = wmc_prob(&mut self.manager, &c.exact);
                self.max_stats = self
                    .max_stats
                    .largest_of(&stats.expect("output dists should be non-empty"));
                let expectations = Expectations::new(pq, query_result.clone());
                self.current_exp.mut_add(&expectations);

                let stop = Instant::now();
                let duration = stop.duration_since(self.start);
                Some(SamplingResult {
                    step,
                    stats: self.max_stats.clone(),
                    expectations: self.current_exp.clone(),
                    duration,
                    weight: pq,
                })
            }
            Err(e) => None,
        }
    }
}
