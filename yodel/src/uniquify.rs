use crate::data::CompileError;
use crate::grammar::*;
use crate::typeinf::grammar::ProgramInferable;
use grammar::*;
use std::collections::HashMap;
use std::fmt::Debug;

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

    impl fmt::Display for UniqueId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "#{}", self.0)
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Uniquify;

    impl ξ<Uniquify> for AVarExt<SVal> {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for AValExt<SVal> {
        type Ext = ();
    }
    impl ξ<Uniquify> for AVarExt<EVal> {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for AValExt<EVal> {
        type Ext = ();
    }
    pub type AnfUnq<X> = Anf<Uniquify, X>;

    impl ξ<Uniquify> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for EPrjExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for EProdExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for ELetInExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for EIteExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for EFlipExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for EObserveExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for SObserveExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for ESampleExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for SAnfExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for SLetInExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for SSeqExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for SIteExt {
        type Ext = ();
    }
    impl ξ<Uniquify> for SBernExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for SDiscreteExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for SUniformExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for SNormalExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for SBetaExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for SDirichletExt {
        type Ext = UniqueId;
    }
    impl ξ<Uniquify> for SExactExt {
        type Ext = ();
    }

    pub type EExprUnq = EExpr<Uniquify>;
    pub type SExprUnq = SExpr<Uniquify>;
    pub type ProgramUnq = Program<Uniquify>;
}

pub struct SymEnv {
    pub names: HashMap<String, UniqueId>,
    pub gensym: u64,
    read_only: bool,
}
impl Default for SymEnv {
    fn default() -> Self {
        Self {
            names: Default::default(),
            gensym: 0,
            read_only: true,
        }
    }
}

impl SymEnv {
    fn _fresh(&mut self, ovar: Option<String>) -> UniqueId {
        let sym = self.gensym;
        self.gensym += 1;
        let var = ovar.unwrap_or(format!("_{sym}"));
        self.names.insert(var, UniqueId(sym));
        UniqueId(sym)
    }
    fn fresh(&mut self) -> UniqueId {
        self._fresh(None)
    }
    fn get_var(&self, var: String) -> Option<UniqueId> {
        self.names.get(&var).copied()
    }
    fn get_or_create(&mut self, var: String) -> Result<UniqueId, CompileError> {
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
    pub fn uniquify_anf<Val>(&mut self, a: &AnfUD<Val>) -> Result<AnfUnq<Val>, CompileError>
    where
        AVarExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<UD, Ext = ()>,
        AValExt<Val>: ξ<Uniquify, Ext = ()> + ξ<UD, Ext = ()>,
        Val: Debug + Clone + PartialEq,
    {
        use crate::grammar::Anf::*;
        match a {
            AVar(_, s) => {
                let uid = self.get_or_create(s.to_string())?;
                Ok(AVar(uid, s.to_string()))
            }
            AVal(_, b) => Ok(AVal((), b.clone())),
            And(bl, br) => Ok(And(
                Box::new(self.uniquify_anf(bl)?),
                Box::new(self.uniquify_anf(br)?),
            )),
            Or(bl, br) => Ok(Or(
                Box::new(self.uniquify_anf(bl)?),
                Box::new(self.uniquify_anf(br)?),
            )),
            Neg(bl) => Ok(Neg(Box::new(self.uniquify_anf(bl)?))),
            _ => todo!(),
        }
    }
    pub fn uniquify_anfs<Val: Clone>(
        &mut self,
        anfs: &[AnfUD<Val>],
    ) -> Result<Vec<AnfUnq<Val>>, CompileError>
    where
        AVarExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<UD, Ext = ()>,
        AValExt<Val>: ξ<Uniquify, Ext = ()> + ξ<UD, Ext = ()>,
        Val: Debug + Clone + PartialEq,
    {
        anfs.iter().map(|a| self.uniquify_anf(a)).collect()
    }

    pub fn uniquify_eexpr(&mut self, e: &EExprUD) -> Result<EExprUnq, CompileError> {
        use crate::grammar::EExpr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.uniquify_anf(a)?))),
            EPrj(_ty, i, a) => Ok(EPrj((), *i, Box::new(self.uniquify_anf(a)?))),
            EProd(_ty, anfs) => Ok(EProd((), self.uniquify_anfs(anfs)?)),
            ELetIn(_ty, s, ebound, ebody) => {
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
            EIte(_ty, cond, t, f) => Ok(EIte(
                (),
                Box::new(self.uniquify_anf(cond)?),
                Box::new(self.uniquify_eexpr(t)?),
                Box::new(self.uniquify_eexpr(f)?),
            )),
            EFlip(_, param) => Ok(EFlip(self.fresh(), *param)),
            EObserve(_, a) => {
                let anf = self.uniquify_anf(a)?;
                Ok(EObserve((), Box::new(anf)))
            }
            ESample(_, e) => Ok(ESample((), Box::new(self.uniquify_sexpr(e)?))),
        }
    }
    pub fn uniquify_sexpr(&mut self, e: &SExprUD) -> Result<SExprUnq, CompileError> {
        use crate::grammar::SExpr::*;
        match e {
            SAnf(_, a) => Ok(SAnf((), Box::new(self.uniquify_anf(a)?))),
            SSeq(_ty, e0, e1) => Ok(SSeq(
                (),
                Box::new(self.uniquify_sexpr(e0)?),
                Box::new(self.uniquify_sexpr(e1)?),
            )),
            SLetIn(_ty, s, ebound, ebody) => {
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
            SIte(_ty, cond, t, f) => Ok(SIte(
                (),
                Box::new(self.uniquify_anf(cond)?),
                Box::new(self.uniquify_sexpr(t)?),
                Box::new(self.uniquify_sexpr(f)?),
            )),
            SBern(_, param) => {
                let param = self.uniquify_anf(param)?;
                Ok(SBern(self.fresh(), Box::new(param)))
            }
            SUniform(_, lo, hi) => {
                let lo = self.uniquify_anf(lo)?;
                let hi = self.uniquify_anf(hi)?;
                Ok(SUniform(self.fresh(), Box::new(lo), Box::new(hi)))
            }
            SNormal(_, mean, var) => {
                let mean = self.uniquify_anf(mean)?;
                let var = self.uniquify_anf(var)?;
                Ok(SNormal(self.fresh(), Box::new(mean), Box::new(var)))
            }
            SBeta(_, a, b) => {
                let a = self.uniquify_anf(a)?;
                let b = self.uniquify_anf(b)?;
                Ok(SBeta(self.fresh(), Box::new(a), Box::new(b)))
            }
            SDiscrete(_, ps) => {
                let ps = self.uniquify_anfs(ps)?;
                Ok(SDiscrete(self.fresh(), ps))
            }
            SDirichlet(_, ps) => {
                let ps = self.uniquify_anfs(ps)?;
                Ok(SDirichlet(self.fresh(), ps))
            }
            SObserve(_, a, e) => Ok(SObserve(
                (),
                Box::new(self.uniquify_anf(a)?),
                Box::new(self.uniquify_sexpr(e)?),
            )),

            SExact(_, e) => Ok(SExact((), Box::new(self.uniquify_eexpr(e)?))),
        }
    }

    pub fn uniquify(&mut self, p: &ProgramUD) -> Result<(ProgramUnq, MaxUniqueId), CompileError> {
        let mx = MaxUniqueId(self.gensym);
        match p {
            Program::SBody(e) => {
                let eann = self.uniquify_sexpr(e)?;
                Ok((Program::SBody(eann), mx))
            }
            Program::EBody(e) => {
                let eann = self.uniquify_eexpr(e)?;
                Ok((Program::EBody(eann), mx))
            }
        }
    }
}

pub fn pipeline(p: &ProgramInferable) -> Result<(ProgramUnq, MaxUniqueId), CompileError> {
    let p = crate::typecheck::pipeline(p)?;
    let mut senv = SymEnv::default();
    senv.uniquify(&p)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::*;
    use crate::grammar::*;
    use crate::typecheck::pipeline;
    use crate::typeinf::grammar::{EExprInferable, ProgramInferable};
    use crate::*;
    use tracing::*;
    use tracing_test::traced_test;

    #[test]
    fn invalid_observe() {
        let res = SymEnv::default().uniquify(&pipeline(&program!(observe!(b!("x")))).unwrap());
        assert!(res.is_err());
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
        let mut senv = SymEnv::default();
        let res = senv.uniquify(&pipeline(&mk(b!("x"))).unwrap());
        assert!(res.is_ok());
        assert!(senv.gensym == 6);
    }
}
