use crate::data::{errors, CompileError, Result};
use crate::grammar::*;
use grammar::*;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use tracing::*;

pub mod grammar {
    use super::*;
    use rsdd::repr::var_label::*;
    use std::fmt;
    use std::fmt::*;

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct MaxUniqueId(pub u64);

    #[derive(Clone, Copy, Eq, Hash, PartialEq, Debug)]
    pub struct UniqueId(pub u64);
    impl UniqueId {
        pub fn from_lbl(lbl: VarLabel) -> UniqueId {
            UniqueId(lbl.value())
        }
        pub fn as_lbl(&self) -> VarLabel {
            VarLabel::new(self.0)
        }
    }

    #[derive(Clone, Copy, Eq, Hash, PartialEq, Debug)]
    pub struct FnId(pub UniqueId);

    #[derive(Clone, Copy, Eq, Hash, PartialEq, Debug)]
    pub struct FnCall(pub FnId, pub u64);

    #[derive(Debug, Clone, Hash, Eq, PartialEq)]
    pub struct FnCounts {
        pub num_calls: u64,
        pub num_uids: u64,
        pub iterates: bool,
    }
    impl Default for FnCounts {
        fn default() -> Self {
            Self {
                num_calls: 0,
                num_uids: 0,
                iterates: false,
            }
        }
    }
    impl fmt::Display for UniqueId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "#{}", self.0)
        }
    }

    ::ttg::phase!(pub struct Uniquify: {
        ELetInExt: UniqueId,
        SLetInExt: UniqueId,

        AVarExt<EVal>: UniqueId,
        AVarExt<SVal>: UniqueId,

     // APrjExt<SVal>: UniqueId,
     // APrjExt<EVal>: UniqueId,

        // this should actually be (), but just to keep things simple, I'll keep
        // this symmetric with SVal
        ADistExt<EVal>: UniqueId,
        ADistExt<SVal>: UniqueId,
        // Keep this around as we are not /completely/ migrating away from
        // original semantic
        EFlipExt: UniqueId,

        SAppExt: FnCall,
        EAppExt: FnCall,
        EIterateExt: FnId,

        SMapExt: UniqueId,
        SFoldExt: (UniqueId, UniqueId),
        SLambdaExt: Vec<UniqueId>,

    });

    ::ttg::alias!(Uniquify as Unq + (Program, EExpr, SExpr, Anf<Var>));
}

pub struct SymEnv {
    pub names: HashMap<String, VecDeque<UniqueId>>,
    pub scope: VecDeque<HashSet<String>>,
    pub functions: HashMap<String, FnId>, // just going to assume a nice flat structure for a hot minute
    pub fun_stats: HashMap<FnId, FnCounts>,
    pub gensym: u64,
    read_only: bool,
}
impl Default for SymEnv {
    fn default() -> Self {
        Self {
            names: Default::default(),
            scope: Default::default(),
            gensym: 0,
            read_only: true,
            functions: Default::default(),
            fun_stats: Default::default(),
        }
    }
}

impl SymEnv {
    fn current_frame(&mut self) -> &mut HashSet<String> {
        match self.scope.iter().last() {
            Some(_) => (),
            None => {
                self.scope.push_back(HashSet::new());
            }
        }
        self.scope.iter_mut().last().unwrap()
    }
    fn _fresh(&mut self, ovar: Option<String>) -> UniqueId {
        let sym = self.gensym;
        self.gensym += 1;

        let var = match ovar {
            None => format!("_{sym}"),
            Some(var) => {
                // push named variables onto the current stack frame
                // println!("var {var:?}");
                let cur = self.current_frame();
                cur.insert(var.clone());
                var
            }
        };

        match self.names.get_mut(&var) {
            None => {
                // push identifier onto variable stack
                let mut stack = VecDeque::new();
                stack.push_back(UniqueId(sym));
                self.names.insert(var, stack);
            }
            Some(stack) => {
                stack.push_back(UniqueId(sym));
            }
        }
        UniqueId(sym)
    }
    fn scope_push(&mut self) {
        self.scope.push_back(Default::default());
    }
    fn scope_pop(&mut self) {
        let frame = self.scope.pop_back().unwrap();
        for var in frame {
            match self.names.get_mut(&var) {
                None => panic!("impossible! misuse of API"),
                Some(stack) => {
                    stack.pop_back();
                }
            }
        }
    }
    fn fresh(&mut self) -> UniqueId {
        self._fresh(None)
    }
    fn fresh_function(&mut self, name: String) -> FnId {
        let id = FnId(self.fresh());
        self.functions.insert(name, id);
        self.fun_stats.insert(id, Default::default());
        id
    }
    fn call_function(&mut self, f: &str) -> FnCall {
        let id = self.functions.get(f).expect("function {f} is not defined");
        let cs = self
            .fun_stats
            .get_mut(id)
            .expect("counters always  created at same time as ");
        cs.num_calls += 1;
        FnCall(*id, cs.num_calls)
    }
    fn call_function_iter(&mut self, f: &str) -> FnId {
        let id = self.functions.get(f).expect("function {f} is not defined");
        let cs = self
            .fun_stats
            .get_mut(id)
            .expect("counters always  created at same time as ");
        cs.iterates = true;
        *id
    }

    fn get_var(&self, var: String) -> Option<UniqueId> {
        self.names.get(&var)?.iter().last().copied()
    }
    fn get_or_create(&mut self, var: String) -> Result<UniqueId> {
        let osym = self.get_var(var.clone());
        match osym {
            None => {
                if self.read_only {
                    Err(CompileError::Generic(format!(
                        "error: encountered unbound variable \"{}\"",
                        var
                    )))
                } else {
                    Ok(self._fresh(Some(var)))
                }
            }
            Some(sym) => Ok(sym),
        }
    }

    fn uniquify_anf_binop<X: Debug + PartialEq + Clone>(
        &mut self,
        l: &AnfUD<X>,
        r: &AnfUD<X>,
        op: impl Fn(Box<AnfUnq<X>>, Box<AnfUnq<X>>) -> AnfUnq<X>,
    ) -> Result<AnfUnq<X>>
    where
        AVarExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        AValExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = ()>,
        // APrjExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        ADistExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
    {
        Ok(op(
            Box::new(self.uniquify_anf(l)?),
            Box::new(self.uniquify_anf(r)?),
        ))
    }
    fn uniquify_anf_binop_<X: Debug + PartialEq + Clone>(
        &mut self,
        l: &AnfUD<X>,
        r: &AnfUD<X>,
        mut op: impl FnMut(UniqueId, Box<AnfUnq<X>>, Box<AnfUnq<X>>) -> AnfUnq<X>,
    ) -> Result<AnfUnq<X>>
    where
        AVarExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        // APrjExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        AValExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = ()>,
        ADistExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
    {
        let id = self.fresh();
        Ok(op(
            id,
            Box::new(self.uniquify_anf(l)?),
            Box::new(self.uniquify_anf(r)?),
        ))
    }

    fn uniquify_anf_vec<X: Debug + PartialEq + Clone>(
        &mut self,
        xs: &[AnfUD<X>],
    ) -> Result<Vec<AnfUnq<X>>>
    where
        AVarExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        AValExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = ()>,
        // APrjExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        ADistExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
    {
        xs.iter().map(|a| self.uniquify_anf(a)).collect()
    }

    fn uniquify_anf<X: Debug + PartialEq + Clone>(&mut self, a: &AnfUD<X>) -> Result<AnfUnq<X>>
    where
        AVarExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        AValExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = ()>,
        // APrjExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        ADistExt<X>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
    {
        use crate::grammar::Anf::*;
        match a {
            AVar(_, s) => {
                let uid = self.get_or_create(s.to_string())?;
                Ok(AVar(uid, s.to_string()))
            }
            AVal(_, v) => Ok(AVal((), v.clone())),

            // Booleans
            And(l, r) => self.uniquify_anf_binop(l, r, And),
            Or(l, r) => self.uniquify_anf_binop(l, r, Or),
            Neg(bl) => Ok(Neg(Box::new(self.uniquify_anf(bl)?))),

            // Numerics
            Plus(l, r) => self.uniquify_anf_binop(l, r, Plus),
            Minus(l, r) => self.uniquify_anf_binop(l, r, Minus),
            Mult(l, r) => self.uniquify_anf_binop(l, r, Mult),
            Div(l, r) => self.uniquify_anf_binop(l, r, Div),

            // Ord
            GT(l, r) => self.uniquify_anf_binop(l, r, GT),
            LT(l, r) => self.uniquify_anf_binop(l, r, LT),
            GTE(l, r) => self.uniquify_anf_binop(l, r, GTE),
            LTE(l, r) => self.uniquify_anf_binop(l, r, LTE),
            EQ(l, r) => self.uniquify_anf_binop(l, r, EQ),

            // [x]; (l,r); x[0]
            AnfVec(xs) => Ok(AnfVec(self.uniquify_anf_vec(xs)?)),
            AnfProd(xs) => Ok(AnfProd(self.uniquify_anf_vec(xs)?)),
            AnfPrj(var, ix) => Ok(AnfPrj(
                Box::new(self.uniquify_anf(var)?),
                Box::new(self.uniquify_anf(ix)?),
            )),
            // AnfPrj(_, s, ix) => {
            //     let uid = self.get_or_create(s.to_string())?;
            //     Ok(AnfPrj(uid, s.to_string(), Box::new(self.uniquify_anf(ix)?)))
            // }

            // Distributions
            AnfBernoulli(_, x) => Ok(AnfBernoulli(self.fresh(), Box::new(self.uniquify_anf(x)?))),
            AnfPoisson(_, x) => Ok(AnfPoisson(self.fresh(), Box::new(self.uniquify_anf(x)?))),
            AnfUniform(_, l, r) => self.uniquify_anf_binop_(l, r, AnfUniform),
            AnfNormal(_, l, r) => self.uniquify_anf_binop_(l, r, AnfNormal),
            AnfBeta(_, l, r) => self.uniquify_anf_binop_(l, r, AnfBeta),
            AnfDiscrete(_, xs) => Ok(AnfDiscrete(self.fresh(), self.uniquify_anf_vec(xs)?)),
            AnfDirichlet(_, xs) => Ok(AnfDirichlet(self.fresh(), self.uniquify_anf_vec(xs)?)),
        }
    }
    pub fn uniquify_anfs<Val: Clone>(&mut self, anfs: &[AnfUD<Val>]) -> Result<Vec<AnfUnq<Val>>>
    where
        AVarExt<Val>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        AValExt<Val>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = ()>,
        // APrjExt<Val>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        ADistExt<Val>: ξ<UD, Ext = ()> + ξ<Uniquify, Ext = UniqueId>,
        Val: Debug + Clone + PartialEq,
    {
        anfs.iter().map(|a| self.uniquify_anf(a)).collect()
    }

    pub fn uniquify_eexpr(&mut self, e: &EExprUD) -> Result<EExprUnq> {
        use crate::grammar::EExpr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.uniquify_anf(a)?))),
            // EPrj(_, i, a) => Ok(EPrj(
            //     (),
            //     Box::new(self.uniquify_anf(i)?),
            //     Box::new(self.uniquify_anf(a)?),
            // )),
            // EProd(_, anfs) => Ok(EProd((), self.uniquify_anfs(anfs)?)),
            ELetIn(_, s, ebound, ebody) => {
                trace!("elet {:?}", s);
                // too lazy to do something smarter
                self.read_only = false;
                let v = self.get_or_create(s.to_string())?;
                self.read_only = true;
                Ok(ELetIn(
                    v,
                    s.clone(),
                    Box::new(self.uniquify_eexpr(ebound)?),
                    Box::new(self.uniquify_eexpr(ebody)?),
                ))
            }
            EIte(_, cond, t, f) => Ok(EIte(
                (),
                Box::new(self.uniquify_anf(cond)?),
                Box::new(self.uniquify_eexpr(t)?),
                Box::new(self.uniquify_eexpr(f)?),
            )),
            EFlip(_, p) => {
                let p = self.uniquify_anf(p)?;
                Ok(EFlip(self.fresh(), Box::new(p)))
            }
            EApp(_, f, args) => {
                let fcall = self.call_function(f);
                Ok(EApp(fcall, f.clone(), self.uniquify_anfs(args)?))
            }
            EIterate(_, f, init, k) => {
                let init = self.uniquify_anf(init)?;
                let k = self.uniquify_anf(k)?;
                let fid = self.call_function_iter(f);
                Ok(EIterate(fid, f.clone(), Box::new(init), Box::new(k)))
            }
            EObserve(_, a, rst) => Ok(EObserve(
                (),
                Box::new(self.uniquify_anf(a)?),
                Box::new(self.uniquify_eexpr(rst)?),
            )),
            ESample(_, e) => Ok(ESample((), Box::new(self.uniquify_sexpr(e)?))),
            EDiscrete(_, _) => errors::erased(Uniquify, "discrete"),
        }
    }
    pub fn uniquify_sexpr(&mut self, e: &SExprUD) -> Result<SExprUnq> {
        use crate::grammar::SExpr::*;
        match e {
            SAnf(_, a) => Ok(SAnf((), Box::new(self.uniquify_anf(a)?))),
            SLambda(_, args, body) => {
                self.scope_push();
                let body = self.uniquify_sexpr(body)?;
                let ids = args
                    .iter()
                    .map(|a| self.get_or_create(a.to_string()))
                    .collect::<Result<Vec<UniqueId>>>()?;
                self.scope_pop();
                Ok(SLambda(ids, args.clone(), Box::new(body)))
            }
            SSeq(_, e0, e1) => Ok(SSeq(
                (),
                Box::new(self.uniquify_sexpr(e0)?),
                Box::new(self.uniquify_sexpr(e1)?),
            )),
            SLetIn(_, s, ebound, ebody) => {
                // too lazy to do something smarter
                self.read_only = false;
                let v = self.get_or_create(s.to_string())?;
                self.read_only = true;
                Ok(SLetIn(
                    v,
                    s.clone(),
                    Box::new(self.uniquify_sexpr(ebound)?),
                    Box::new(self.uniquify_sexpr(ebody)?),
                ))
            }
            SIte(_, cond, t, f) => Ok(SIte(
                (),
                Box::new(self.uniquify_anf(cond)?),
                Box::new(self.uniquify_sexpr(t)?),
                Box::new(self.uniquify_sexpr(f)?),
            )),
            SObserve(_, a, e, rst) => Ok(SObserve(
                (),
                Box::new(self.uniquify_anf(a)?),
                Box::new(self.uniquify_anf(e)?),
                Box::new(self.uniquify_sexpr(rst)?),
            )),

            SExact(_, e) => Ok(SExact((), Box::new(self.uniquify_eexpr(e)?))),

            SMap(_, arg, map, xs) => {
                self.scope_push();
                let arg_id = self.get_or_create(arg.to_string())?;
                let map = self.uniquify_sexpr(map)?;
                self.scope_pop();
                let xs = self.uniquify_anf(xs)?;
                Ok(SMap(arg_id, arg.clone(), Box::new(map), Box::new(xs)))
            }
            SFold(_, init, accum, arg, fold, xs) => {
                let init = self.uniquify_anf(init)?;
                self.scope_push();
                let accum_id = self.get_or_create(accum.to_string())?;
                let arg_id = self.get_or_create(arg.to_string())?;
                let fold = self.uniquify_sexpr(fold)?;
                self.scope_pop();
                let xs = self.uniquify_anf(xs)?;
                Ok(SFold(
                    (accum_id, arg_id),
                    Box::new(init),
                    accum.clone(),
                    arg.clone(),
                    Box::new(fold),
                    Box::new(xs),
                ))
            }
            SWhile(_, guard, body) => Ok(SWhile(
                (),
                Box::new(self.uniquify_anf(guard)?),
                Box::new(self.uniquify_sexpr(body)?),
            )),
            SApp(_, f, args) => {
                let fcall = self.call_function(f);
                Ok(SApp(fcall, f.clone(), self.uniquify_anfs(args)?))
            }
            SSample(_, dist) => Ok(SSample((), Box::new(self.uniquify_sexpr(dist)?))),
            SLetSample(_, _, _, _) => errors::erased(Uniquify, "let-sample"),
        }
    }
    pub fn uniquify_efun(&mut self, f: &Function<EExprUD>) -> Result<Function<EExprUnq>> {
        let name = f.name.clone();
        self.read_only = false;
        let arguments = self.uniquify_anfs(&f.arguments)?;
        self.read_only = true;
        let body = self.uniquify_eexpr(&f.body)?;
        let returnty = f.returnty.clone();

        Ok(Function {
            name,
            arguments,
            body,
            returnty,
        })
    }
    pub fn uniquify_sfun(&mut self, f: &Function<SExprUD>) -> Result<Function<SExprUnq>> {
        let name = f.name.clone();
        self.read_only = false;
        let arguments = self.uniquify_anfs(&f.arguments)?;
        self.read_only = true;
        let body = self.uniquify_sexpr(&f.body)?;
        let returnty = f.returnty.clone();

        Ok(Function {
            name,
            arguments,
            body,
            returnty,
        })
    }

    pub fn uniquify(&mut self, p: &ProgramUD) -> Result<(ProgramUnq, MaxUniqueId)> {
        // tracing::debug!("uniquify-ing");
        match p {
            Program::SBody(e) => {
                let eann = self.uniquify_sexpr(e)?;
                Ok((Program::SBody(eann), MaxUniqueId(self.gensym)))
            }
            Program::EBody(e) => {
                let eann = self.uniquify_eexpr(e)?;
                Ok((Program::EBody(eann), MaxUniqueId(self.gensym)))
            }
            Program::EDefine(f, p) => {
                let id =
                    self.fresh_function(f.name.clone().expect("defined functions must be named"));
                let start_ids = self.gensym.clone();
                let f = self.uniquify_efun(f)?;
                let end_ids = self.gensym.clone();
                let cs = self.fun_stats.get_mut(&id).expect("I just made that!");
                cs.num_uids = end_ids - start_ids;
                let (p, mx) = self.uniquify(p)?;
                Ok((Program::EDefine(f, Box::new(p)), mx))
            }
            Program::SDefine(f, p) => {
                let id =
                    self.fresh_function(f.name.clone().expect("defined functions must be named"));
                let start_ids = self.gensym.clone();
                let f = self.uniquify_sfun(f)?;
                let end_ids = self.gensym.clone();
                let cs = self.fun_stats.get_mut(&id).expect("I just made that!");
                cs.num_uids = end_ids - start_ids;

                let (p, mx) = self.uniquify(p)?;
                Ok((Program::SDefine(f, Box::new(p)), mx))
            }
        }
    }
}

pub fn pipeline(p: &crate::typeinf::grammar::ProgramInferable) -> Result<(ProgramUnq, SymEnv)> {
    let p = crate::typecheck::pipeline(p)?;
    let mut senv = SymEnv::default();
    let (p, mx) = senv.uniquify(&p)?;
    Ok((p, senv))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::*;
    use crate::grammar::*;
    use crate::typecheck::pipeline;
    use crate::typeinf::grammar::{EExprInferable, ProgramInferable};
    use crate::uniquify::grammar::{EExprUnq, ProgramUnq};
    use crate::*;
    use tracing::*;
    use tracing_test::traced_test;

    #[test]
    #[traced_test]
    fn invalid_observe() {
        println!("----------------------------------------------------");
        let res = SymEnv::default().uniquify(&pipeline(&program!(observe!(b!("x")))).unwrap());
        assert!(res.is_err());
        println!("----------------------------------------------------");
        let mk = |ret: EExprInferable| {
            Program::EBody(lets![
                "x" ; b!() ;= flip!(1/3);
                "y" ; b!() ;= sample!(
                    lets![
                        "x0" ; b!() ;= flip!(1/5);
                        ...? b!("x0" || "x") ; b!()
                    ]);
               "_" ; b!() ;= observe!(b!("x" || "y")); // is this a problem?

               ...? ret ; b!()
            ])
        };
        let res = SymEnv::default().uniquify(&pipeline(&mk(b!("l"))).unwrap());
        assert!(res.is_err());
        println!("----------------------------------------------------");
        let mut senv = SymEnv::default();
        let res = senv.uniquify(&pipeline(&mk(b!("x"))).unwrap());
        assert!(res.is_ok());
        assert!(senv.gensym == 6);
    }
}
