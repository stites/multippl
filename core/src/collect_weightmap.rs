/// colllect the weight map in advance of compilation.
/// TODO: if there is an observe inside a sample's subprogram, we need to remove
/// the entry after exiting the sample, or change the weight to the constant weight.
use crate::annotate::grammar::*;
use crate::compile::CompileError;
use crate::grammar::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub struct Weight {
    pub lo: f64,
    pub hi: f64,
}
impl Weight {
    pub fn as_tuple(&self) -> (f64, f64) {
        (self.lo, self.hi)
    }
    pub fn from_high(hi: f64) -> Weight {
        Weight { lo: 1.0 - hi, hi }
    }
    pub fn constant() -> Weight {
        Self::from_high(0.5)
    }
}

pub struct WeightEnv {
    weights: HashMap<Var, Weight>,
}

impl WeightEnv {
    pub fn new() -> Self {
        Self {
            weights: HashMap::new(),
        }
    }
    pub fn weightmap(&self) -> rsdd::repr::wmc::WmcParams<f64> {
        let mut wmc_params = rsdd::repr::wmc::WmcParams::new(0.0, 1.0);
        for (var, weight) in self.weights.iter() {
            match var.label {
                Some(label) => wmc_params.set_weight(label, weight.lo, weight.hi),
                None => panic!("impossible"),
            }
        }
        wmc_params
    }
    pub fn collect_over_expr(&mut self, e: &ExprAnn) -> Result<(), CompileError> {
        use crate::grammar::Expr::*;
        match e {
            ELetIn(id, s, ebound, ebody) => {
                self.collect_over_expr(ebound)?;
                self.collect_over_expr(ebody)
            }
            EIte(_ty, cond, t, f) => {
                self.collect_over_expr(t)?;
                self.collect_over_expr(f)
            }
            EFlip(var, param) => {
                self.weights.insert(var.clone(), Weight::from_high(*param));
                Ok(())
            }
            ESample(_, e) => self.collect_over_expr(e),
            _ => Ok(()),
        }
    }

    pub fn collect_over_program(&mut self, p: &ProgramAnn) -> Result<WmcParams<f64>, CompileError> {
        match p {
            Program::Body(e) => {
                let _ = self.collect_over_expr(e)?;
                let weights = self.weightmap();
                Ok(weights)
            }
        }
    }
}

pub fn collect_weightmap(p: &ProgramAnn) -> Result<WmcParams<f64>, CompileError> {
    WeightEnv::new().collect_over_program(&p)
}
