use crate::data::CompileError;
use crate::grammar::*;
use crate::typeinf::grammar::ProgramInferable;
use crate::uniquify::grammar::*;
use grammar::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use std::collections::HashMap;
use std::collections::HashSet;

pub type InvMap = HashMap<NamedVar, HashSet<BddVar>>;

pub mod grammar {
    use super::*;
    use std::fmt;
    use std::fmt::*;

    #[derive(Clone, Hash, Eq, PartialEq)]
    pub struct BddVar {
        pub id: UniqueId,
        pub label: VarLabel,
        pub provenance: Option<NamedVar>,
    }
    impl Debug for BddVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.provenance {
                None => f.write_str(&format!("BddVar(#{}, L{})", self.id.0, self.label.value())),
                Some(n) => f.write_str(&format!(
                    "BddVar(#{}, L{}, from:{})",
                    self.id.0,
                    self.label.value(),
                    n.name
                )),
            }
        }
    }

    impl BddVar {
        pub fn id(&self) -> UniqueId {
            self.id
        }
        pub fn new(id: UniqueId, label: VarLabel, provenance: Option<NamedVar>) -> Self {
            BddVar {
                id,
                label,
                provenance,
            }
        }
    }

    #[derive(Clone, Hash, Eq, PartialEq)]
    pub struct NamedVar {
        pub id: UniqueId,
        pub name: String,
    }
    impl Debug for NamedVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(&format!("NamedVar(#{}, {})", self.id.0, self.name))
        }
    }
    pub fn named(id: u64, name: &str) -> NamedVar {
        NamedVar::new(UniqueId(id), name.to_string())
    }
    impl NamedVar {
        pub fn id(&self) -> UniqueId {
            self.id
        }
        pub fn new(id: UniqueId, name: String) -> Self {
            NamedVar { id, name }
        }
    }

    // FIXME: this should be an enum of BddVar or NamedVar
    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    pub enum Var {
        Bdd(BddVar),
        Named(NamedVar),
    }
    impl Var {
        pub fn new_bdd(id: UniqueId, label: VarLabel, provenance: Option<NamedVar>) -> Self {
            Var::Bdd(BddVar {
                id,
                label,
                provenance,
            })
        }
        pub fn new_named(id: UniqueId, name: String) -> Self {
            Var::Named(NamedVar { id, name })
        }
        pub fn debug_id(&self) -> String {
            match self {
                Var::Bdd(v) => format!("{}", v.id),
                Var::Named(v) => v.name.clone(),
            }
        }
        pub fn unsafe_label(&self) -> VarLabel {
            match self {
                Var::Bdd(v) => v.label,
                Var::Named(_) => panic!("shame on you!"),
            }
        }
        pub fn id(&self) -> UniqueId {
            match self {
                Var::Bdd(v) => v.id,
                Var::Named(v) => v.id,
            }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Annotated;

    impl ξ<Annotated> for AVarExt<SVal> {
        type Ext = NamedVar;
    }
    impl ξ<Annotated> for AValExt<SVal> {
        type Ext = ();
    }
    impl ξ<Annotated> for AVarExt<EVal> {
        type Ext = NamedVar;
    }
    impl ξ<Annotated> for AValExt<EVal> {
        type Ext = ();
    }
    pub type AnfAnn<Val> = Anf<Annotated, Val>;

    impl ξ<Annotated> for EAnfExt {
        type Ext = ();
    }
    impl ξ<Annotated> for EPrjExt {
        // sampleable
        type Ext = ();
    }
    impl ξ<Annotated> for EProdExt {
        type Ext = ();
    }
    impl ξ<Annotated> for ELetInExt {
        // vars up/down
        // binders are "sample-able"
        type Ext = NamedVar;
    }
    impl ξ<Annotated> for EIteExt {
        type Ext = ();
    }
    impl ξ<Annotated> for EFlipExt {
        // vars up/down
        // flip is sample-able
        type Ext = BddVar;
    }
    impl ξ<Annotated> for EObserveExt {
        type Ext = ();
    }
    impl ξ<Annotated> for SObserveExt {
        type Ext = ();
    }

    impl ξ<Annotated> for ESampleExt {
        type Ext = ();
    }

    impl ξ<Annotated> for SAnfExt {
        type Ext = ();
    }
    impl ξ<Annotated> for SLetInExt {
        type Ext = NamedVar;
    }
    impl ξ<Annotated> for SSeqExt {
        type Ext = ();
    }
    impl ξ<Annotated> for SIteExt {
        type Ext = ();
    }
    // >>>>>>>>>>>>> do nothing special for sampled distributions
    impl ξ<Annotated> for SBernExt {
        type Ext = ();
    }
    impl ξ<Annotated> for SDiscreteExt {
        type Ext = ();
    }
    impl ξ<Annotated> for SUniformExt {
        type Ext = ();
    }
    impl ξ<Annotated> for SNormalExt {
        type Ext = ();
    }
    impl ξ<Annotated> for SBetaExt {
        type Ext = ();
    }
    impl ξ<Annotated> for SDirichletExt {
        type Ext = ();
    }
    // <<<<<<<<<<<<< do nothing special for sampled distributions

    impl ξ<Annotated> for SExactExt {
        type Ext = ();
    }

    pub type EExprAnn = EExpr<Annotated>;
    pub type SExprAnn = SExpr<Annotated>;
    pub type ProgramAnn = Program<Annotated>;
}

pub struct LabelEnv {
    lblsym: u64,
    subst_var: HashMap<UniqueId, Var>,
    letpos: Option<NamedVar>,
}

impl LabelEnv {
    pub fn new() -> Self {
        Self {
            lblsym: 0,
            subst_var: HashMap::new(),
            letpos: None,
        }
    }
    pub fn max_varlabel_val(&self) -> u64 {
        self.lblsym
    }

    pub fn linear_var_order(&self) -> rsdd::repr::var_order::VarOrder {
        rsdd::repr::var_order::VarOrder::linear_order(self.max_varlabel_val() as usize)
    }
    pub fn get_inv(&self) -> HashMap<NamedVar, HashSet<BddVar>> {
        let mut inv: HashMap<NamedVar, HashSet<BddVar>> = HashMap::new();
        for (_, var) in self.subst_var.iter() {
            match &var {
                Var::Named(_) => continue,
                Var::Bdd(v) => match &v.provenance {
                    None => continue,
                    Some(prov) => match inv.get_mut(&prov) {
                        None => {
                            inv.insert(prov.clone(), HashSet::from([v.clone()]));
                        }
                        Some(vs) => {
                            vs.insert(v.clone());
                        }
                    },
                },
            }
        }
        inv
    }

    fn fresh(&mut self) -> VarLabel {
        let sym = self.lblsym;
        self.lblsym += 1;
        VarLabel::new(sym)
    }

    pub fn get_var(&self, id: &UniqueId) -> Result<Var, CompileError> {
        match self.subst_var.get(id) {
            None => Err(CompileError::Generic(format!("symbol {id} not in scope"))),
            Some(x) => Ok(x.clone()),
        }
    }

    pub fn annotate_anf<Val: Clone>(&mut self, a: &AnfUnq<Val>) -> Result<AnfAnn<Val>, CompileError>
    where
        AVarExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = NamedVar>,
        AValExt<Val>: ξ<Uniquify, Ext = ()> + ξ<Annotated, Ext = ()>,
    {
        use crate::grammar::Anf::*;
        match a {
            AVar(uid, s) => {
                let var = self.get_var(uid)?;
                match var {
                    Var::Named(nvar) => Ok(AVar(nvar, s.to_string())),
                    Var::Bdd(_) => panic!("bdd vars are never referenced by source code!"),
                }
            }
            AVal(_, b) => Ok(AVal((), b.clone())),
            And(bl, br) => Ok(And(
                Box::new(self.annotate_anf(bl)?),
                Box::new(self.annotate_anf(br)?),
            )),
            Or(bl, br) => Ok(Or(
                Box::new(self.annotate_anf(bl)?),
                Box::new(self.annotate_anf(br)?),
            )),
            Neg(bl) => Ok(Neg(Box::new(self.annotate_anf(bl)?))),
            _ => todo!(),
        }
    }
    pub fn annotate_anfs<Val: Clone>(
        &mut self,
        anfs: &[AnfUnq<Val>],
    ) -> Result<Vec<AnfAnn<Val>>, CompileError>
    where
        AVarExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = NamedVar>,
        AValExt<Val>: ξ<Uniquify, Ext = ()> + ξ<Annotated, Ext = ()>,
    {
        anfs.iter().map(|a| self.annotate_anf(a)).collect()
    }
    pub fn annotate_eexpr(&mut self, e: &EExprUnq) -> Result<EExprAnn, CompileError> {
        use crate::grammar::EExpr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.annotate_anf(a)?))),
            EPrj(_, i, a) => Ok(EPrj((), *i, Box::new(self.annotate_anf(a)?))),
            EProd(_, anfs) => Ok(EProd((), self.annotate_anfs(anfs)?)),
            ELetIn(id, s, ebound, ebody) => {
                let nvar = NamedVar {
                    id: *id,
                    name: s.to_string(),
                };
                let var = Var::Named(nvar.clone());
                self.letpos = Some(nvar.clone());
                // self.weights.insert(var.clone(), Weight::constant());
                self.subst_var.insert(*id, var.clone());
                Ok(ELetIn(
                    nvar,
                    s.clone(),
                    Box::new(self.annotate_eexpr(ebound)?),
                    Box::new(self.annotate_eexpr(ebody)?),
                ))
            }
            EIte(_ty, cond, t, f) => Ok(EIte(
                (),
                Box::new(self.annotate_anf(cond)?),
                Box::new(self.annotate_eexpr(t)?),
                Box::new(self.annotate_eexpr(f)?),
            )),
            EFlip(id, param) => {
                let lbl = self.fresh();
                let var = BddVar::new(*id, lbl, self.letpos.clone());
                self.subst_var.insert(*id, Var::Bdd(var.clone()));
                Ok(EFlip(var, *param))
            }
            EObserve(_, a) => {
                let anf = self.annotate_anf(a)?;
                Ok(EObserve((), Box::new(anf)))
            }
            ESample(_, e) => Ok(ESample((), Box::new(self.annotate_sexpr(e)?))),
        }
    }
    pub fn annotate_sexpr(&mut self, e: &SExprUnq) -> Result<SExprAnn, CompileError> {
        use crate::grammar::SExpr::*;
        match e {
            SAnf(_, a) => Ok(SAnf((), Box::new(self.annotate_anf(a)?))),
            SSeq(_, e0, e1) => Ok(SSeq(
                (),
                Box::new(self.annotate_sexpr(e0)?),
                Box::new(self.annotate_sexpr(e1)?),
            )),
            SLetIn(id, s, ebound, ebody) => {
                let nvar = NamedVar {
                    id: *id,
                    name: s.to_string(),
                };
                let var = Var::Named(nvar.clone());
                self.letpos = Some(nvar.clone());
                // self.weights.insert(var.clone(), Weight::constant());
                self.subst_var.insert(*id, var.clone());
                Ok(SLetIn(
                    nvar,
                    s.clone(),
                    Box::new(self.annotate_sexpr(ebound)?),
                    Box::new(self.annotate_sexpr(ebody)?),
                ))
            }
            SIte(_ty, cond, t, f) => Ok(SIte(
                (),
                Box::new(self.annotate_anf(cond)?),
                Box::new(self.annotate_sexpr(t)?),
                Box::new(self.annotate_sexpr(f)?),
            )),

            SBern(_, param) => {
                let param = self.annotate_anf(param)?;
                Ok(SBern((), Box::new(param)))
            }
            SUniform(_, lo, hi) => {
                let lo = self.annotate_anf(lo)?;
                let hi = self.annotate_anf(hi)?;
                Ok(SUniform((), Box::new(lo), Box::new(hi)))
            }
            SNormal(_, mean, var) => {
                let mean = self.annotate_anf(mean)?;
                let var = self.annotate_anf(var)?;
                Ok(SNormal((), Box::new(mean), Box::new(var)))
            }
            SBeta(_, a, b) => {
                let a = self.annotate_anf(a)?;
                let b = self.annotate_anf(b)?;
                Ok(SBeta((), Box::new(a), Box::new(b)))
            }
            SDiscrete(_, ps) => {
                let ps = self.annotate_anfs(ps)?;
                Ok(SDiscrete((), ps))
            }
            SDirichlet(_, ps) => {
                let ps = self.annotate_anfs(ps)?;
                Ok(SDirichlet((), ps))
            }

            SExact(_, e) => Ok(SExact((), Box::new(self.annotate_eexpr(e)?))),
        }
    }
    #[allow(clippy::type_complexity)]
    pub fn annotate(
        &mut self,
        p: &ProgramUnq,
    ) -> Result<
        (
            ProgramAnn,
            VarOrder,
            HashMap<UniqueId, Var>,
            HashMap<NamedVar, HashSet<BddVar>>,
            u64,
        ),
        CompileError,
    > {
        match p {
            Program::SBody(e) => {
                let eann = self.annotate_sexpr(e)?;
                let order = self.linear_var_order();
                let inv = self.get_inv();
                let mx = self.max_varlabel_val();
                Ok((Program::SBody(eann), order, self.subst_var.clone(), inv, mx))
            }
            Program::EBody(e) => {
                let eann = self.annotate_eexpr(e)?;
                let order = self.linear_var_order();
                let inv = self.get_inv();
                let mx = self.max_varlabel_val();
                Ok((Program::EBody(eann), order, self.subst_var.clone(), inv, mx))
            }
        }
    }
}

pub fn pipeline(
    p: &ProgramInferable,
) -> Result<
    (
        ProgramAnn,
        VarOrder,
        HashMap<UniqueId, Var>,
        HashMap<NamedVar, HashSet<BddVar>>,
        u64,
    ),
    CompileError,
> {
    let p = crate::uniquify::pipeline(p)?.0;
    let mut lenv = LabelEnv::new();
    lenv.annotate(&p)
}
