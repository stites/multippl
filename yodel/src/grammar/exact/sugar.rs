#![allow(non_camel_case_types)]

use crate::typeinf::grammar::{EExprInferable, Inferable};
/// Trees-that-grow style grammar to unify type-checking, analysis, and
/// compilation. going to be honest it's pretty atrocious in rust.
use crate::*;

use crate::grammar::program::Program;

use ::core::fmt;
use ::core::fmt::Debug;
use itertools::Itertools;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::string::String;

trait Desugar<As> {
    fn desugar(&self) -> As;
}

#[derive(Debug, Clone, PartialEq)]
pub enum EAnfSugar {
    EAnfPrim(Box<Anf<Inferable, EVal>>),
    EInteger(usize),
}
impl Desugar<Anf<Inferable, EVal>> for EAnfSugar {
    fn desugar(&self) -> Anf<Inferable, EVal> {
        use EAnfSugar::*;
        match self {
            EAnfPrim(a) => *a.clone(),
            EInteger(i) => todo!(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ESugar {
    // SampleSugar(Box<SSugar<X>>),  // (avoid the extra wrapping for now)
    Prim(EExpr<Inferable>),
    LetIn(String, Box<ESugar>, Box<ESugar>),
    Ite(Box<Anf<Inferable, EVal>>, Box<ESugar>, Box<ESugar>),
    Sample(Box<SSugar>),

    // integer support
    Discrete(Vec<f64>),     // => if-then-else chain returning a one-hot encoding
    IntAnf(Box<EAnfSugar>), // => integer desugaring
    IntPrj(Box<EAnfSugar>, Box<Anf<Inferable, EVal>>), // => TODO: EPrj using a variable for the index

    // iterate(f, init, k)
    Iterate(String, Box<EAnfSugar>, Box<EAnfSugar>),
}

impl Desugar<EExpr<Inferable>> for ESugar {
    fn desugar(&self) -> EExpr<Inferable> {
        use EAnfSugar::*;
        match self {
            ESugar::Prim(e) => e.clone(),
            ESugar::LetIn(v, bind, body) => EExpr::ELetIn(
                None,
                v.clone(),
                Box::new(bind.desugar()),
                Box::new(body.desugar()),
            ),
            ESugar::Ite(p, t, f) => EExpr::EIte(
                None,
                p.clone(),
                Box::new(t.desugar()),
                Box::new(f.desugar()),
            ),
            ESugar::Discrete(params) => discrete::from_params(params),
            ESugar::Sample(s) => EExpr::ESample((), Box::new(s.desugar())),
            ESugar::IntAnf(a) => match *a.clone() {
                EAnfPrim(anf) => EExpr::EAnf((), anf.clone()),
                EInteger(i) => EExpr::EAnf((), Box::new(integers::as_prod(i))),
            },
            ESugar::IntPrj(ix_anf, prod_anf) => match *ix_anf.clone() {
                EInteger(i) => EExpr::EPrj(None, i, prod_anf.clone()),
                EAnfPrim(anf) => todo!("not yet useful"),
            },
            ESugar::Iterate(f, init, kanf) => match *kanf.clone() {
                EInteger(i) => EExpr::EApp(None, f.clone(), vec![init.desugar()]),
                EAnfPrim(anf) => todo!("not yet useful"),
            },
        }
    }
}

impl Lang for ESugar {
    type Ty = ETy;
    type Function = crate::grammar::Function<Self>;
    type Anf = EAnfSugar;
    type Val = EVal;
}
impl Desugar<Anf<Inferable, SVal>> for Anf<Inferable, SVal> {
    fn desugar(&self) -> Anf<Inferable, SVal> {
        self.clone()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum SSugar {
    Prim(SExpr<Inferable>),
    LetSample(String, Box<SSugar>, Box<SSugar>), // let x = ~(<sexpr>) in <sexpr>
    Exact(Box<ESugar>),
}

impl Lang for SSugar {
    type Ty = STy;
    type Function = crate::grammar::Function<SSugar>;
    type Anf = crate::grammar::Anf<Inferable, SVal>;
    type Val = SVal;
}

impl Desugar<SExpr<Inferable>> for SSugar {
    fn desugar(&self) -> SExpr<Inferable> {
        use SExpr::*;
        match self {
            SSugar::Prim(e) => e.clone(),
            SSugar::Exact(e) => SExact((), Box::new(e.desugar())),
            SSugar::LetSample(s, e0, e1) => SLetIn(
                None,
                s.clone(),
                Box::new(SSample((), Box::new(e0.desugar()))),
                Box::new(e1.desugar()),
            ),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ProgramSugar {
    Exact(ESugar),
    Sample(SSugar),
    ExactFn(Function<ESugar>, Box<ProgramSugar>),
    SampleFn(Function<SSugar>, Box<ProgramSugar>),
}

// TTG!(impl Desugar<Function<EExpr<Inferable>>> for Function<ESugar> {
//     pub fn desugar(self) -> Function<EExpr<Inferable>> {
//         Function {
//             name: self.name.clone(),
//             arguments: self
//                 .arguments
//                 .clone()
//                 .into_iter()
//                 .map(|x| x.desugar())
//                 .collect(),
//             body: self.body.clone().desugar(),
//         }
//     }
// });

impl<Sugar, Inf> Desugar<Function<Inf>> for Function<Sugar>
where
    Sugar: Lang + Debug + Clone + PartialEq,
    Inf: Lang + Debug + Clone + PartialEq,
    <Inf as Lang>::Anf: PartialEq + Clone + Debug,
    <Sugar as Lang>::Anf: PartialEq + Debug + Clone,
    <Sugar as Lang>::Anf: Desugar<<Inf as Lang>::Anf>,
    Sugar: Desugar<Inf>,
{
    fn desugar(&self) -> Function<Inf> {
        Function {
            name: self.name.clone(),
            arguments: self
                .arguments
                .clone()
                .into_iter()
                .map(|x| x.desugar())
                .collect(),
            body: self.body.clone().desugar(),
        }
    }
}

impl Desugar<Program<Inferable>> for ProgramSugar {
    fn desugar(&self) -> Program<Inferable> {
        use ProgramSugar::*;
        match self {
            Exact(body) => Program::EBody(body.desugar()),
            Sample(body) => Program::SBody(body.desugar()),
            ExactFn(f, body) => Program::EDefine(f.desugar(), Box::new(body.desugar())),
            SampleFn(f, body) => Program::SDefine(f.desugar(), Box::new(body.desugar())),
        }
    }
}
pub mod integers {
    use super::*;
    pub fn as_prod(i: usize) -> Anf<Inferable, EVal> {
        let ty = vec![ETy::EBool; i];
        let mut val = vec![EVal::EBool(false); i - 1];
        val.push(EVal::EBool(true));
        Anf::AVal((), EVal::EProd(val))
    }
    // pub fn from_prod(e: &Anf<Inferable, EVal>) -> usize {
    //     match e {
    //         Anf::AVal((), EVal::EProd(vs)) => {
    //             assert_eq!(
    //                 vs.iter()
    //                     .map(|v| v.as_bool().expect("value should be one-hot encoding") as usize)
    //                     .sum::<usize>(),
    //                 1,
    //                 "attempting to convert prod which is not one-hot encoded, should be a type-checking error"
    //             );
    //             vs.iter().enumerate().find(|(ix, x)| x.is_true()).unwrap().0
    //         }
    //         _ => panic!("api misuse"),
    //     }
    // }
    pub fn integer_op(e: &Anf<Inferable, EVal>) {
        todo!()
    }
    pub fn prod2usize(p: Anf<Inferable, EVal>) -> Option<usize> {
        match p {
            Anf::AVal(_, EVal::EProd(prod)) => {
                let (ix, sum) =
                    prod.iter()
                        .enumerate()
                        .fold(Some((0, 0)), |memo, (ix, x)| match (memo, x) {
                            (Some((one_ix, tot)), EVal::EBool(b)) => {
                                Some(if *b { (ix, tot + 1) } else { (one_ix, tot) })
                            }
                            _ => None,
                        })?;

                if sum != 1 {
                    None
                } else {
                    Some(ix)
                }
            }
            _ => None,
        }
    }
}
pub mod discrete {
    use crate::grammar::{Anf, EExpr, ETy, EVal};
    use crate::typeinf::grammar::{EExprInferable, Inferable};
    use itertools::*;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    fn params2partitions(params: &Vec<f64>) -> Vec<f64> {
        // assumes normalized probs, just to keep this subroutine simple
        params.iter().fold(vec![1.0], |mut zs, p| {
            let z = zs.last().unwrap() - p;
            if z > 0.0 {
                zs.push(z);
            }
            zs
        })
    }
    fn normalize(params: &Vec<f64>) -> Vec<f64> {
        let z = params.iter().sum::<f64>();
        if z == 1.0 {
            params.clone()
        } else {
            params.iter().map(|p| p / z).collect()
        }
    }
    fn params2probs(params: &Vec<f64>) -> Vec<f64> {
        // put this somewhere else
        let params = normalize(params);
        let partitions = params2partitions(&params);
        params
            .into_iter()
            .zip(partitions.into_iter())
            .map(|(phat, z)| phat / z)
            .collect()
    }
    fn hash_discrete(params: &Vec<f64>) -> String {
        let mut hasher = DefaultHasher::new();
        let s = params.iter().map(|p| format!("{}", p)).join("-");
        s.hash(&mut hasher);
        (&format!("{:x}", hasher.finish())[..6]).to_string()
    }

    fn mkvar(v: String) -> Anf<Inferable, EVal> {
        Anf::AVar(Some(ETy::EBool), v)
    }

    fn mkguard(ids: Vec<String>) -> Anf<Inferable, EVal> {
        let mut var = None;
        for id in ids {
            let nxt = match var {
                None => Anf::Neg(Box::new(mkvar(id))),
                Some(prev) => Anf::And(Box::new(prev), Box::new(Anf::Neg(Box::new(mkvar(id))))),
            };
            var = Some(nxt);
        }
        var.unwrap()
    }

    fn mkdiscrete_var(flip_id: String, seen_ids: Vec<String>) -> Anf<Inferable, EVal> {
        let flip = mkvar(flip_id);
        if seen_ids.len() == 0 {
            flip
        } else {
            let seen = mkguard(seen_ids);
            Anf::And(Box::new(seen), Box::new(flip))
        }
    }

    pub fn params2statements(params: &Vec<f64>) -> Vec<(String, EExprInferable)> {
        let discrete_id = hash_discrete(params);
        let names = params
            .iter()
            .enumerate()
            .map(|(ix, _)| format!("{}", ix))
            .collect_vec();
        params2named_statements(&discrete_id, &names, params)
    }

    pub fn float2eanf(f: f64) -> Anf<Inferable, EVal> {
        Anf::AVal((), EVal::EFloat(f))
    }

    pub fn params2named_statements(
        namespace: &String,
        names: &Vec<String>,
        params: &Vec<f64>,
    ) -> Vec<(String, EExprInferable)> {
        let n = params.len();
        assert_eq!(names.len(), n);
        let probs = params2probs(params);

        let mut flips: Vec<(String, EExprInferable)> = names
            .iter()
            .zip(probs.iter())
            .map(|(n, p)| {
                (
                    format!("{}_{}_flip", namespace, n),
                    EExpr::EFlip((), Box::new(float2eanf(*p))),
                )
            })
            .collect_vec();
        flips.pop();

        let mut vars: Vec<(String, EExprInferable)> = vec![];
        let mut seen: Vec<String> = vec![];

        for (name, (flip_lbl, flip)) in names.iter().zip(flips.into_iter()) {
            let var_lbl = format!("{}_{}", namespace, name);
            let var_exp = mkdiscrete_var(flip_lbl.clone(), seen.clone());
            vars.push((flip_lbl.clone(), flip));
            vars.push((var_lbl.clone(), EExpr::EAnf((), Box::new(var_exp))));
            seen.push(var_lbl);
        }
        vars.push((
            format!("{}_{}", namespace, names.last().unwrap()),
            EExpr::EAnf((), Box::new(mkguard(seen))),
        ));
        vars
    }

    pub fn from_params(params: &Vec<f64>) -> EExprInferable {
        let discrete_id = hash_discrete(params);
        let names = params
            .iter()
            .enumerate()
            .map(|(ix, _)| format!("{}", ix))
            .collect_vec();
        from_named_params(&discrete_id, &names, params)
    }

    pub fn from_named_params(
        namespace: &String,
        names: &Vec<String>,
        params: &Vec<f64>,
    ) -> EExprInferable {
        assert!(
            params.len() > 1,
            "parameters must be normalized with cardinality of >1"
        );
        if params.len() == 2 {
            assert!(
                (params.iter().sum::<f64>() - 1.0).abs() < 1e-30,
                "Bernoulli must be normalized"
            );
            EExpr::EFlip((), Box::new(float2eanf(params[0])))
        } else {
            let mut statements = params2named_statements(namespace, names, params);
            let mut binding = statements.last().unwrap().1.clone();
            statements.pop();
            let thetype = Some(crate::typecheck::grammar::LetInTypes {
                bindee: ETy::EBool,
                body: ETy::EProd(vec![ETy::EBool; params.len()]),
            });
            for (label, bindee) in statements.into_iter().rev() {
                binding =
                    EExpr::ELetIn(thetype.clone(), label, Box::new(bindee), Box::new(binding));
            }
            binding
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        pub fn test_params2partitions() {
            let params = vec![0.1, 0.4, 0.5];
            let partitions = params2partitions(&params);
            assert_eq!(partitions, vec![1.0, 0.9, 0.5]);
            let params = vec![0.1, 0.4, 0.2, 0.3];
            let partitions = params2partitions(&params);
            assert_eq!(partitions, vec![1.0, 0.9, 0.5, 0.3]);
        }
        #[test]
        pub fn test_params2probs() {
            let params = vec![0.1, 0.4, 0.5];
            let probs = params2probs(&params);
            assert_eq!(probs, vec![0.1, 0.4 / 0.9, 1.0]);
            let params = vec![0.1, 0.4, 0.2, 0.3];
            let probs = params2probs(&params);
            assert_eq!(probs, vec![0.1, 0.4 / 0.9, 0.2 / 0.5, 1.0]);
            let params = vec![1.0, 4.0, 2.0, 3.0];
            let probs = params2probs(&params);
            assert_eq!(probs, vec![0.1, 0.4 / 0.9, 0.2 / 0.5, 1.0]);
        }
        #[test]
        pub fn test_params2hash() {
            let params = vec![0.1, 0.4, 0.5];
            let probs = hash_discrete(&params);
            assert_eq!(probs, "22f7f9");
            let params = vec![0.1, 0.4, 0.2, 0.3];
            let probs = hash_discrete(&params);
            assert_eq!(probs, "1501f9");
            let params = vec![1.0, 4.0, 2.0, 3.0];
            let probs = hash_discrete(&params);
            assert_eq!(probs, "4b912b");
        }
        #[test]
        pub fn test_mkguard_for_discrete_var() {
            let bindee = mkguard(vec![String::from("seen1"), String::from("seen2")]);
            let seen1 = Anf::Neg(Box::new(Anf::AVar(Some(ETy::EBool), String::from("seen1"))));
            let seen2 = Anf::Neg(Box::new(Anf::AVar(Some(ETy::EBool), String::from("seen2"))));
            assert_eq!(
                bindee,
                Anf::And(Box::new(seen1.clone()), Box::new(seen2.clone()),)
            );
            let bindee = mkguard(vec![
                String::from("seen1"),
                String::from("seen2"),
                String::from("seen3"),
            ]);
            let seen3 = Anf::Neg(Box::new(Anf::AVar(Some(ETy::EBool), String::from("seen3"))));
            assert_eq!(
                bindee,
                Anf::And(
                    Box::new(Anf::And(Box::new(seen1), Box::new(seen2))),
                    Box::new(seen3),
                )
            );
        }
        #[test]
        pub fn test_params2statements() {
            let params = vec![0.1, 0.4, 0.5];
            let mut statements = params2statements(&params);
            for (s, e) in statements.iter() {
                println!("let {} := {:?}", s, e);
            }
            statements.pop();
            let last_flip = statements.last().unwrap();
            // assert!(false);
        }
        #[test]
        pub fn test_params2bindings() {
            let params = vec![0.1, 0.4, 0.5];
            let bindings = from_params(&params);
            println!("{:?}", bindings);
            // assert!(false);
        }
    }
}
