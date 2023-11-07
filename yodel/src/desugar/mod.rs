#![allow(non_camel_case_types)]

use crate::*;

use crate::data::errors;
use crate::typeinf::grammar::{EExprInferable, Inferable};
use crate::*;

use crate::grammar::program::Program;

use core::fmt;
use core::fmt::Debug;
use discrete::*;
use itertools::Itertools;
use rsdd::builder::bdd_plan::BddPlan;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::string::String;

pub fn desugar_eval(v: &EVal) -> Result<EVal> {
    use EVal::*;
    match v {
        EProd(vs) => Ok(EProd(
            vs.iter().map(desugar_eval).collect::<Result<Vec<EVal>>>()?,
        )),
        // EInteger(i) => Ok(integers::as_onehot(*i)),
        _ => Ok(v.clone()),
    }
}

fn desugar_eanf_binop(
    l: &AnfUD<EVal>,
    r: &AnfUD<EVal>,
    op: impl Fn(Box<AnfUD<EVal>>, Box<AnfUD<EVal>>) -> AnfUD<EVal>,
) -> Result<AnfUD<EVal>> {
    Ok(op(Box::new(desugar_eanf(l)?), Box::new(desugar_eanf(r)?)))
}
fn desugar_eanf_vec(xs: &[AnfUD<EVal>]) -> Result<Vec<AnfUD<EVal>>> {
    xs.iter().map(desugar_eanf).collect()
}
pub fn desugar_eanf(a: &AnfUD<EVal>) -> Result<AnfUD<EVal>> {
    use crate::Anf::*;
    match a {
        AVar(_, s) => Ok(AVar((), s.clone())),
        AVal(_, v) => Ok(AVal((), desugar_eval(v)?)),
        // Booleans
        And(l, r) => desugar_eanf_binop(l, r, And),
        Or(l, r) => desugar_eanf_binop(l, r, Or),
        Neg(p) => Ok(Neg(Box::new(desugar_eanf(p)?))),

        // Numerics
        Plus(l, r) => desugar_eanf_binop(l, r, Plus),
        Minus(l, r) => desugar_eanf_binop(l, r, Minus),
        Mult(l, r) => desugar_eanf_binop(l, r, Mult),
        Div(l, r) => desugar_eanf_binop(l, r, Div),

        // Ord
        GT(l, r) => desugar_eanf_binop(l, r, GT),
        LT(l, r) => desugar_eanf_binop(l, r, LT),
        GTE(l, r) => desugar_eanf_binop(l, r, GTE),
        LTE(l, r) => desugar_eanf_binop(l, r, LTE),
        EQ(l, r) => desugar_eanf_binop(l, r, EQ),

        // [x]; (l,r); x[0]
        AnfVec(xs) => errors::not_in_exact(),
        AnfProd(xs) => Ok(AnfProd(desugar_eanf_vec(xs)?)),
        AnfPrj(var, ix) => Ok(AnfPrj(
            Box::new(desugar_eanf(var)?),
            Box::new(desugar_eanf(ix)?),
        )),

        AnfBernoulli(_, x) => errors::not_in_exact(),
        AnfPoisson(_, x) => errors::not_in_exact(),
        AnfUniform(_, l, r) => errors::not_in_exact(),
        AnfNormal(_, l, r) => errors::not_in_exact(),
        AnfBeta(_, l, r) => errors::not_in_exact(),
        AnfDiscrete(_, xs) => errors::not_in_exact(),
        AnfDirichlet(_, xs) => errors::not_in_exact(),
    }
}

fn desugar_fun<ExprIn, ExprOut, Val, T>(
    mut translate_expr: impl FnMut(&ExprIn) -> Result<ExprOut>,
    f: &Function<ExprIn>,
) -> Result<Function<ExprOut>>
where
    <ExprIn as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprOut as Lang>::Anf: PartialEq + Debug + Clone,
    <ExprIn as Lang>::Ty: PartialEq + Debug + Clone,
    <ExprOut as Lang>::Ty: PartialEq + Debug + Clone,
    ExprIn: PartialEq + Debug + Clone + Lang<Anf = Anf<UD, Val>, Ty = T>,
    ExprOut: PartialEq + Debug + Clone + Lang<Anf = Anf<UD, Val>, Ty = T>,
    AVarExt<Val>: ξ<UD, Ext = ()>,
    // APrjExt<Val>: ξ<UD, Ext = ()>,
    ADistExt<Val>: ξ<UD, Ext = ()>,
    AValExt<Val>: ξ<UD, Ext = ()>,
    Val: Debug + PartialEq + Clone,
{
    Ok(Function {
        name: f.name.clone(),
        arguments: f.arguments.clone(),
        body: translate_expr(&f.body)?,
        returnty: f.returnty.clone(),
    })
}

pub struct SugarMagicEnv {
    pub discretes: HashMap<String, Vec<String>>,
    pub current_ebind: Option<String>,
}

impl SugarMagicEnv {
    pub fn new() -> Self {
        Self {
            discretes: HashMap::new(),
            current_ebind: None,
        }
    }

    pub fn desugar(&mut self, p: &grammar::ProgramUD) -> Result<ProgramUD> {
        use Program::*;
        match p {
            EBody(e) => Ok(EBody(self.desugar_eexpr(e)?)),
            SBody(e) => Ok(SBody(self.desugar_sexpr(e)?)),
            EDefine(f, p) => Ok(EDefine(
                desugar_fun(|x| self.desugar_eexpr(x), f)?,
                Box::new(self.desugar(p)?),
            )),
            SDefine(f, p) => Ok(SDefine(
                desugar_fun(|x| self.desugar_sexpr(x), f)?,
                Box::new(self.desugar(p)?),
            )),
        }
    }

    pub fn desugar_eexpr(&mut self, e: &grammar::EExprUD) -> Result<EExprUD> {
        use crate::grammar::EExpr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(desugar_eanf(a)?))),
            ELetIn(_, s, ebound, ebody) => {
                self.current_ebind = Some(s.clone());
                Ok(ELetIn(
                    (),
                    s.clone(),
                    Box::new(self.desugar_eexpr(ebound)?),
                    Box::new(self.desugar_eexpr(ebody)?),
                ))
            }
            EIte(_, cond, t, f) => Ok(EIte(
                (),
                Box::new(desugar_eanf(cond)?),
                Box::new(self.desugar_eexpr(t)?),
                Box::new(self.desugar_eexpr(f)?),
            )),
            EFlip(_, param) => Ok(EFlip((), Box::new(desugar_eanf(param)?))),
            EObserve(_, a, e) => {
                match observed_int(a) {
                    None => Ok(EObserve(
                        (),
                        Box::new(desugar_eanf(a)?),
                        Box::new(self.desugar_eexpr(e)?),
                    )),
                    Some((v, i)) => {
                        match self.discretes.get(&v) {
                            None => panic!("type checking for observed discrete"),
                            Some(vs) => {
                                let rebind = AnfUD::<EVal>::AVar((), vs[i].clone());
                                // let reobserve = AnfUD::<EVal>::EQ(rebind, EVal::EBdd(BddPtr::PtrTrue));
                                Ok(EObserve(
                                    (),
                                    Box::new(rebind),
                                    Box::new(self.desugar_eexpr(e)?),
                                ))
                            }
                        }
                    }
                }
            }
            EApp(_, f, args) => Ok(EApp((), f.clone(), desugar_eanf_vec(args)?)),
            EIterate(_, f, init, times) => Ok(EIterate(
                (),
                f.clone(),
                Box::new(desugar_eanf(init)?),
                Box::new(desugar_eanf(times)?),
            )),
            EDiscrete(_, args) => match &self.current_ebind {
                None => from_params(&args),
                Some(s) => {
                    let (vs, e) = from_default_named_params_h(s, &args)?;
                    let fs = vs
                        .into_iter()
                        .filter(|l| !l.ends_with("_flip"))
                        .collect_vec();
                    self.discretes.insert(s.clone(), fs);
                    Ok(e)
                }
            },
            ESample(_, e) => Ok(ESample((), Box::new(self.desugar_sexpr(e)?))),
        }
    }
    pub fn desugar_sexpr(&mut self, e: &grammar::SExprUD) -> Result<SExprUD> {
        use crate::grammar::SExpr::*;
        match e {
            SAnf(_, _) => Ok(e.clone()),
            SLetIn(_, s, ebound, ebody) => Ok(SLetIn(
                (),
                s.clone(),
                Box::new(self.desugar_sexpr(ebound)?),
                Box::new(self.desugar_sexpr(ebody)?),
            )),
            SSeq(_, e0, e1) => Ok(SSeq(
                (),
                Box::new(self.desugar_sexpr(e0)?),
                Box::new(self.desugar_sexpr(e1)?),
            )),
            SIte(_, cond, t, f) => Ok(SIte(
                (),
                cond.clone(),
                Box::new(self.desugar_sexpr(t)?),
                Box::new(self.desugar_sexpr(f)?),
            )),
            SMap(_, arg, map, xs) => Ok(SMap(
                (),
                arg.clone(),
                Box::new(self.desugar_sexpr(map)?),
                xs.clone(),
            )),
            SFold(_, init, accum, arg, fold, xs) => Ok(SFold(
                (),
                init.clone(),
                accum.clone(),
                arg.clone(),
                Box::new(self.desugar_sexpr(fold)?),
                xs.clone(),
            )),
            SWhile(_, guard, body) => Ok(SWhile(
                (),
                guard.clone(),
                Box::new(self.desugar_sexpr(body)?),
            )),

            SApp(_, f, args) => Ok(e.clone()),
            SLambda(_, args, body) => Ok(SLambda(
                (),
                args.clone(),
                Box::new(self.desugar_sexpr(body)?),
            )),

            SSample(_, dist) => Ok(SSample((), Box::new(self.desugar_sexpr(dist)?))),
            SObserve(_, a, b, c) => Ok(SObserve(
                (),
                a.clone(),
                b.clone(),
                Box::new(self.desugar_sexpr(c)?),
            )),

            SExact(_, e) => Ok(SExact((), Box::new(self.desugar_eexpr(e)?))),

            // gone!
            SLetSample(_, var, dist, rest) => Ok(SLetIn(
                (),
                var.clone(),
                Box::new(SSample((), Box::new(self.desugar_sexpr(dist)?))),
                Box::new(self.desugar_sexpr(rest)?),
            )),
        }
    }
}

pub mod integers {
    use super::*;
    pub fn as_onehot(i: usize) -> EVal {
        let ty = vec![ETy::EBool; i];
        match i {
            0 => EVal::EProd(vec![EVal::EBdd(BddPtr::PtrFalse); 8]),
            _ => {
                let mut val = vec![EVal::EBdd(BddPtr::PtrFalse); i - 1];
                val.push(EVal::EBdd(BddPtr::PtrTrue));
                EVal::EProd(val)
            }
        }
    }
    pub fn from_prod(vs: &[EVal]) -> Result<usize> {
        let (tot, int) = vs.iter().enumerate().fold(Ok((0, 0)), |acc, (ix, v)| {
            let (tot, int) = acc?;
            match v {
                EVal::EBdd(BddPtr::PtrTrue) => Ok((tot + 1, ix)),
                EVal::EBdd(BddPtr::PtrFalse) => Ok((tot, int)),
                _ => errors::generic("prod is not one-hot encoded"),
            }
        })?;
        if tot > 1 {
            errors::generic("(type-error) attempting to convert prod which is not one-hot encoded")
        } else if tot == 0 {
            Ok(0)
        } else {
            println!("tot: {tot}, int: {int}");
            Ok(int + 1)
        }
    }
    pub fn from_prod_val(p: &EVal) -> Result<usize> {
        match p {
            EVal::EProd(vs) => from_prod(vs),
            _ => errors::generic(
                "(type-error) attempting to convert prod which is not one-hot encoded",
            ),
        }
    }
    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        fn test_prop() {
            for x in [0, 1, 5, 10] {
                let oh = as_onehot(x);
                println!("{oh:?}");
                assert_eq!(from_prod_val(&oh).unwrap(), x);
            }
        }
    }
    // pub fn integer_op(e: &Anf<Inferable, EVal>) {
    //     todo!()
    // }
    // pub fn prod2usize(p: Anf<Inferable, EVal>) -> Option<usize> {
    //     match p {
    //         Anf::AVal(_, EVal::EProd(prod)) => {
    //             let (ix, sum) =
    //                 prod.iter()
    //                     .enumerate()
    //                     .fold(Some((0, 0)), |memo, (ix, x)| match (memo, x) {
    //                         (Some((one_ix, tot)), EVal::EBdd(bdd)) => match bdd {
    //                             BddPtr::PtrTrue => Some((ix, tot + 1)),
    //                             BddPtr::PtrFalse => Some((one_ix, tot)),
    //                             _ => None,

    //                         }
    //                         _ => None,
    //                     })?;

    //             if sum != 1 {
    //                 None
    //             } else {
    //                 Some(ix)
    //             }
    //         }
    //         _ => None,
    //     }
    // }
}

pub mod discrete {
    use crate::grammar::undecorated::*;
    use crate::typeinf::grammar::{EExprInferable, Inferable};
    use crate::*;
    use crate::*;
    use itertools::*;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    use tracing::*;

    /// assumes normalized probs
    fn params2partitions(params: &Vec<AnfUD<EVal>>) -> Vec<AnfUD<EVal>> {
        let mut parts = params
            .iter()
            .fold(vec![Anf::AVal((), EVal::EFloat(1.0))], |mut zs, p| {
                let z = Anf::Minus(Box::new(zs.last().unwrap().clone()), Box::new(p.clone()));
                zs.push(z);
                zs
            });
        parts.pop(); // last element is always 0.
        parts
    }
    fn normalize(params: &Vec<AnfUD<EVal>>) -> Vec<AnfUD<EVal>> {
        let z = params
            .iter()
            .fold(Anf::AVal((), EVal::EFloat(0.0)), |tot, p| {
                Anf::Plus(Box::new(tot.clone()), Box::new(p.clone()))
            });
        params
            .iter()
            .map(|p| Anf::Div(Box::new(p.clone()), Box::new(z.clone())))
            .collect()
    }
    fn params2probs(params: &Vec<AnfUD<EVal>>) -> Vec<AnfUD<EVal>> {
        // put this somewhere else
        let params = normalize(params);
        let partitions = params2partitions(&params);
        params
            .into_iter()
            .zip(partitions.into_iter())
            .map(|(phat, z)| Anf::Div(Box::new(phat.clone()), Box::new(z.clone())))
            .collect()
    }
    fn hash_discrete(params: &Vec<AnfUD<EVal>>) -> String {
        let mut hasher = DefaultHasher::new();
        let s = params.iter().map(|p| format!("{:?}", p)).join("-");
        s.hash(&mut hasher);
        (&format!("{:x}", hasher.finish())[..6]).to_string()
    }

    fn mkvar(v: String) -> Anf<UD, EVal> {
        Anf::AVar((), v)
    }

    fn mkguard(ids: Vec<String>) -> Anf<UD, EVal> {
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

    fn mkdiscrete_var(flip_id: String, seen_ids: Vec<String>) -> Anf<UD, EVal> {
        let flip = mkvar(flip_id);
        if seen_ids.len() == 0 {
            flip
        } else {
            let seen = mkguard(seen_ids);
            Anf::And(Box::new(seen), Box::new(flip))
        }
    }

    pub fn params2statements(
        params: &Vec<AnfUD<EVal>>,
    ) -> (Vec<(String, EExprUD)>, EExprUD, Vec<String>) {
        let discrete_id = hash_discrete(params);
        let names = params
            .iter()
            .enumerate()
            .map(|(ix, _)| format!("{}", ix))
            .collect_vec();
        params2named_statements(&discrete_id, &names, params)
    }

    // pub fn last_conj2vec(f: Anf<UD, EVal>) -> Anf<UD, EVal> {
    //     Anf::AVal((), EVal::EFloat(f))
    // }
    pub fn mk_final_result(finvars: &Vec<String>) -> EExprUD {
        mk_final_conditional_int(finvars)
    }

    pub fn mk_final_tuple(finvars: &Vec<String>) -> EExprUD {
        EExpr::EAnf(
            (),
            Box::new(Anf::AnfProd(
                finvars
                    .into_iter()
                    .map(|s| Anf::AVar((), s.clone()))
                    .collect_vec(),
            )),
        )
    }

    pub fn mk_cond(var: String, t: AnfUD<EVal>, f: EExprUD) -> EExprUD {
        use Anf::*;
        use EExpr::*;
        EIte(
            (),
            Box::new(mkvar(var)),
            Box::new(EAnf((), Box::new(t))),
            Box::new(f),
        )
    }

    pub fn mk_final_conditional_int(finvars: &Vec<String>) -> EExprUD {
        debug!("{finvars:?}");
        let mut vars = finvars.clone();
        let n = vars.len();
        let _last_var_is_unused = vars.pop();
        debug!("len: {n}");
        debug!("last: {:?}", _last_var_is_unused);
        let oh = finvars
            .iter()
            .map(|v| AnfUD::<EVal>::AVar((), v.clone()))
            .collect_vec();
        EExpr::EAnf((), Box::new(AnfUD::<EVal>::AnfProd(oh)))
        // let last = mk_cond(
        //     vars.pop().unwrap(),
        //     Anf::AVal((), EVal::EInteger(n - 1)),
        //     EExpr::EAnf((), Box::new(Anf::AVal((), EVal::EInteger(n)))),
        // );
        // vars.into_iter()
        //     .enumerate()
        //     .rev()
        //     .fold(last, |rest, (i, var)| {
        //         mk_cond(var.to_string(), Anf::AVal((), EVal::EInteger(i + 1)), rest)
        //     })
    }

    pub fn params2named_statements(
        namespace: &String,
        names: &Vec<String>,
        params: &Vec<AnfUD<EVal>>,
    ) -> (Vec<(String, EExprUD)>, EExprUD, Vec<String>) {
        let n = params.len();
        assert_eq!(names.len(), n);
        let probs = params2probs(params);

        let mut flips: Vec<(String, EExprUD)> = names
            .iter()
            .zip(probs.iter())
            .map(|(n, p)| {
                (
                    format!("{}_{}_flip", namespace, n),
                    EExpr::EFlip((), Box::new(p.clone())),
                )
            })
            .collect_vec();
        flips.pop();

        let mut vars: Vec<(String, EExprUD)> = vec![];
        let mut seen: Vec<String> = vec![];
        let mut finvec: Vec<String> = vec![];

        for (name, (flip_lbl, flip)) in names.iter().zip(flips.into_iter()) {
            let var_lbl = format!("{}_{}", namespace, name);
            let var_exp = mkdiscrete_var(flip_lbl.clone(), seen.clone());
            vars.push((flip_lbl.clone(), flip));
            vars.push((var_lbl.clone(), EExpr::EAnf((), Box::new(var_exp))));
            finvec.push(var_lbl.clone());
            seen.push(var_lbl);
        }
        let lastlbl = format!("{}_{}", namespace, names.last().unwrap());

        vars.push((lastlbl.clone(), EExpr::EAnf((), Box::new(mkguard(seen)))));
        finvec.push(lastlbl.clone());
        (vars, mk_final_result(&finvec), finvec)
    }

    pub fn from_params_h(params: &Vec<AnfUD<EVal>>) -> Result<(Vec<String>, EExprUD)> {
        let discrete_id = hash_discrete(params);
        let names = params
            .iter()
            .enumerate()
            .map(|(ix, _)| format!("{}", ix))
            .collect_vec();
        from_named_params_h(&discrete_id, &names, params)
    }

    pub fn from_params(params: &Vec<AnfUD<EVal>>) -> Result<EExprUD> {
        Ok(from_params_h(params)?.1)
    }

    pub fn from_named_params(
        namespace: &String,
        names: &Vec<String>,
        params: &Vec<AnfUD<EVal>>,
    ) -> Result<EExprUD> {
        Ok(from_named_params_h(namespace, names, params)?.1)
    }
    pub fn from_default_named_params_h(
        namespace: &String,
        params: &Vec<AnfUD<EVal>>,
    ) -> Result<(Vec<String>, EExprUD)> {
        let names = params
            .iter()
            .enumerate()
            .map(|(ix, _)| format!("{}", ix))
            .collect_vec();
        from_named_params_h(namespace, &names, params)
    }

    pub fn from_named_params_h(
        namespace: &String,
        names: &Vec<String>,
        params: &Vec<AnfUD<EVal>>,
    ) -> Result<(Vec<String>, EExprUD)> {
        if params.len() < 2 {
            errors::generic("parameters must be normalized with cardinality of >1")
        } else if params.len() == 2 {
            Ok((
                vec![],
                EExpr::EFlip((), Box::new(normalize(params)[0].clone())),
            ))
        } else {
            let (statements, ret, oh) = params2named_statements(namespace, names, params);
            let mut binding = ret.clone();
            for (label, bindee) in statements.into_iter().rev() {
                binding = EExpr::ELetIn((), label, Box::new(bindee), Box::new(binding));
            }
            Ok((oh, binding))
        }
    }

    pub fn observed_int(a: &AnfUD<EVal>) -> Option<(String, usize)> {
        use crate::Anf::*;
        use crate::EVal::*;
        match a {
            EQ(l, r) => match (&**l, &**r) {
                (AVar(_, s), AVal(_, EInteger(i))) => Some((s.clone(), *i)),
                (AVal(_, EInteger(i)), AVar(_, s)) => Some((s.clone(), *i)),
                _ => None,
            },
            _ => None,
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use tracing_test::*;

        fn floats2eanf(params: Vec<f64>) -> Vec<AnfUD<EVal>> {
            params
                .into_iter()
                .map(EVal::EFloat)
                .map(|v| Anf::AVal((), v))
                .collect()
        }
        fn eval(a: &AnfUD<EVal>) -> Result<f64> {
            use Anf::*;
            match a {
                AVal((), EVal::EFloat(f)) => Ok(*f),
                Plus(l, r) => Ok(eval(l)? + eval(r)?),
                Minus(l, r) => Ok(eval(l)? - eval(r)?),
                Mult(l, r) => Ok(eval(l)? * eval(r)?),
                Div(l, r) => Ok(eval(l)? / eval(r)?),
                _ => errors::generic("you definitely messed that one up"),
            }
        }
        fn eanf2floats(params: Vec<AnfUD<EVal>>) -> Result<Vec<f64>> {
            params.iter().map(eval).collect()
        }
        #[test]
        pub fn test_params2partitions() {
            let params = floats2eanf(vec![0.1, 0.4, 0.5]);
            let partitions = params2partitions(&params);
            assert_eq!(eanf2floats(partitions).unwrap(), vec![1.0, 0.9, 0.5]);
            let params = floats2eanf(vec![0.1, 0.4, 0.2, 0.3]);
            let partitions = params2partitions(&params);
            assert_eq!(eanf2floats(partitions).unwrap(), vec![1.0, 0.9, 0.5, 0.3]);
        }
        #[test]
        pub fn test_params2probs() {
            let params = floats2eanf(vec![0.1, 0.4, 0.5]);
            let probs = params2probs(&params);
            assert_eq!(eanf2floats(probs).unwrap(), vec![0.1, 0.4 / 0.9, 1.0]);
            let params = floats2eanf(vec![0.1, 0.4, 0.2, 0.3]);
            let probs = params2probs(&params);
            assert_eq!(
                eanf2floats(probs).unwrap(),
                vec![0.1, 0.4 / 0.9, 0.2 / 0.5, 1.0]
            );
            let params = floats2eanf(vec![1.0, 4.0, 2.0, 3.0]);
            let probs = params2probs(&params);
            assert_eq!(
                eanf2floats(probs).unwrap(),
                vec![0.1, 0.4 / 0.9, 0.2 / 0.5, 1.0]
            );
        }
        #[test]
        pub fn test_params2hash() {
            // more of a gold test
            let params = floats2eanf(vec![0.1, 0.4, 0.5]);
            let hash = hash_discrete(&params);
            assert_eq!(hash, "212e08");
            let params = floats2eanf(vec![0.1, 0.4, 0.2, 0.3]);
            let hash = hash_discrete(&params);
            assert_eq!(hash, "13bd45");
            let params = floats2eanf(vec![1.0, 4.0, 2.0, 3.0]);
            let hash = hash_discrete(&params);
            assert_eq!(hash, "a21a4f");
        }
        #[test]
        pub fn test_mkguard_for_discrete_var() {
            let bindee = mkguard(vec![String::from("seen1"), String::from("seen2")]);
            let seen1 = Anf::Neg(Box::new(Anf::AVar((), String::from("seen1"))));
            let seen2 = Anf::Neg(Box::new(Anf::AVar((), String::from("seen2"))));
            assert_eq!(
                bindee,
                Anf::And(Box::new(seen1.clone()), Box::new(seen2.clone()),)
            );
            let bindee = mkguard(vec![
                String::from("seen1"),
                String::from("seen2"),
                String::from("seen3"),
            ]);
            let seen3 = Anf::Neg(Box::new(Anf::AVar((), String::from("seen3"))));
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
            let params = floats2eanf(vec![0.1, 0.4, 0.5]);
            let (statements, fin, _) = params2statements(&params);
            let mut ls = vec![];
            for (s, e) in statements.iter() {
                ls.push(s);
                println!("let {} := {:?}", s, e);
            }
            println!("in {:?}", fin);
            // assert!(false);
        }
        #[test]
        pub fn test_params2bindings() {
            let params = floats2eanf(vec![0.1, 0.4, 0.5]);
            let bindings = from_params(&params);
            println!("{:?}", bindings);
            // assert!(false);
        }

        #[test]
        #[ignore = "these are no longer valid"]
        pub fn test_desugared_int1() {
            let finvars = vec!["a".to_string(), "b".to_string()];
            let ret = mk_final_conditional_int(&finvars);
            assert_eq!(
                ret,
                mk_cond(
                    "a".to_string(),
                    Anf::AVal((), EVal::EInteger(1)),
                    EExpr::EAnf((), Box::new(Anf::AVal((), EVal::EInteger(2))))
                )
            );
        }

        #[test]
        #[ignore = "these are no longer valid"]
        pub fn test_desugared_int2() {
            let finvars = vec!["a".to_string(), "b".to_string(), "c".to_string()];
            let ret = mk_final_conditional_int(&finvars);
            assert_eq!(
                ret,
                mk_cond(
                    "a".to_string(),
                    Anf::AVal((), EVal::EInteger(1)),
                    mk_cond(
                        "b".to_string(),
                        Anf::AVal((), EVal::EInteger(2)),
                        EExpr::EAnf((), Box::new(Anf::AVal((), EVal::EInteger(3))))
                    )
                )
            );
        }

        #[test]
        #[ignore = "these are no longer valid"]
        pub fn test_desugared_int3() {
            let finvars = vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string(),
            ];
            let ret = mk_final_conditional_int(&finvars);
            assert_eq!(
                ret,
                mk_cond(
                    "a".to_string(),
                    Anf::AVal((), EVal::EInteger(1)),
                    mk_cond(
                        "b".to_string(),
                        Anf::AVal((), EVal::EInteger(2)),
                        mk_cond(
                            "c".to_string(),
                            Anf::AVal((), EVal::EInteger(3)),
                            EExpr::EAnf((), Box::new(Anf::AVal((), EVal::EInteger(4))))
                        )
                    )
                )
            );
        }
    }
}
