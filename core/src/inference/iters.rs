use crate::inference::wmc::*;
use crate::*;
use itertools::*;
use std::time::Duration;
use std::time::Instant;

pub struct SamplingIter {
    pub current_step: usize,
    pub current_exp: Expectations,
    pub max_steps: usize,
    pub opt: crate::Options, // can only be 1
    pub start: Instant,
    pub program: ProgramInferable,
    pub manager: Mgr,
    pub max_stats: WmcStats,
}
impl SamplingIter {
    pub fn new(steps: usize, p: &ProgramInferable, opt: &crate::Options) -> Self {
        let mgr = crate::make_mgr(p);
        Self {
            current_step: 0,
            current_exp: Expectations::empty(),
            max_steps: steps,
            opt: opt.clone(),
            start: Instant::now(),
            program: p.clone(),
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
    pub weight: Importance,
}

impl Iterator for SamplingIter {
    type Item = SamplingResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_step >= self.max_steps {
            return None;
        }
        match crate::runner_with_stdrng(&self.program, &mut self.manager, &self.opt) {
            Ok((cs, inv, rng, pq)) => {
                let mut newopt = self.opt.clone();
                newopt.seed = rng;
                self.opt = newopt;
                let c = cs.as_output()?;
                let step = self.current_step;

                self.current_step += 1;
                let (query_result, stats) = wmc_prob(&mut self.manager, &c);
                self.max_stats = self.max_stats.largest_of(&stats);
                let expectations = Expectations::new(c.importance.clone(), query_result.clone());
                self.current_exp.mut_add(&expectations);

                let stop = Instant::now();
                let duration = stop.duration_since(self.start);
                Some(SamplingResult {
                    step,
                    stats: self.max_stats.clone(),
                    expectations: self.current_exp.clone(),
                    duration,
                    weight: c.importance,
                })
            }
            Err(e) => None,
        }
    }
}
