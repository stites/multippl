use crate::data::{errors, CompileError, Result};
use crate::data::{HashMap, HashSet};
use crate::grammar::*;
use crate::typeinf::grammar::ProgramInferable;
use crate::uniquify::grammar::*;
use crate::DataView;
use grammar::*;
use rsdd::repr::var_label::*;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::wmc::WmcParams;
use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::Hash;
use tracing::*;

pub type InvMap<T> = HashMap<NamedVar, HashSet<T>>;

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct MaxVarLabel(pub u64);

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum QueryType {
    Exact,
    Sample,
}
pub struct AnnotateResult {
    pub program: ProgramAnn,
    pub order: VarOrder,
    pub idmap: HashMap<UniqueId, Var>,
    pub maxbdd: MaxVarLabel,
    pub query_type: QueryType,
    pub does_sample: bool,
    // this is now a lower bound since we can call
    // iterate dynamically now. It's fine since RSDD will grow the number of
    // variables and the order will be linear
}
impl AnnotateResult {
    pub fn new(
        program: ProgramAnn,
        order: VarOrder,
        idmap: HashMap<UniqueId, Var>,
        maxbdd: MaxVarLabel,
        query_type: QueryType,
        does_sample: bool,
    ) -> Self {
        AnnotateResult {
            program,
            order,
            idmap,
            maxbdd,
            query_type,
            does_sample,
        }
    }
}

pub mod grammar {
    use super::*;
    use std::fmt;
    use std::fmt::*;

    #[derive(Clone, Hash, Eq, PartialEq)]
    pub struct BddVar {
        pub id: UniqueId,
        pub label: VarLabel,
        pub provenance: Option<NamedVar>,
        pub fid: Option<FnId>,
    }
    impl Debug for BddVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.provenance {
                None => f.write_str(&format!(
                    "BddVar(#{}, L{}, fid:{:?})",
                    self.id.0,
                    self.label.value(),
                    self.fid
                )),
                Some(n) => f.write_str(&format!(
                    "BddVar(#{}, L{}, from:{}, fid:{:?})",
                    self.id.0,
                    self.label.value(),
                    n.name(),
                    self.fid
                )),
            }
        }
    }

    impl BddVar {
        pub fn id(&self) -> UniqueId {
            self.id
        }
        pub fn new(
            id: UniqueId,
            label: VarLabel,
            provenance: Option<NamedVar>,
            fid: Option<FnId>,
        ) -> Self {
            BddVar {
                id,
                label,
                provenance,
                fid,
            }
        }
    }

    #[derive(Clone, Hash, Eq, PartialEq)]
    pub struct NamedVar {
        id: UniqueId,
        name: String,
        is_data: bool,
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
        pub fn name(&self) -> &str {
            &self.name
        }
        pub fn is_data(&self) -> bool {
            self.is_data
        }
        pub fn new(id: UniqueId, name: String) -> Self {
            NamedVar {
                id,
                name,
                is_data: false,
            }
        }
        pub fn datavar(id: UniqueId, name: String) -> Self {
            NamedVar {
                id,
                name,
                is_data: true,
            }
        }
    }

    #[allow(clippy::enum_variant_names)]
    #[derive(PartialEq, Eq, Clone, Hash, Debug)]
    pub enum SDist {
        SBernoulli,
        SUniform,
        SNormal,
        SPoisson,
        SBeta,
        SDiscrete(usize),
        SDirichlet(usize),
    }
    #[derive(Clone, Hash, Eq, PartialEq)]
    pub struct SampledVar {
        pub id: UniqueId,
        pub dist: SDist, // does this need to get refined later?
        pub provenance: Option<NamedVar>,
    }
    impl Debug for SampledVar {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.provenance {
                None => f.write_str(&format!("SampledVar(#{}, {:?})", self.id.0, self.dist)),
                Some(n) => f.write_str(&format!(
                    "SampledVar(#{}, {:?}, from:{})",
                    self.id.0, self.dist, n.name
                )),
            }
        }
    }
    impl SampledVar {
        pub fn id(&self) -> UniqueId {
            self.id
        }
        pub fn new(id: UniqueId, dist: SDist, provenance: Option<NamedVar>) -> Self {
            SampledVar {
                id,
                dist,
                provenance,
            }
        }
    }

    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    pub enum Var {
        Bdd(BddVar),
        Named(NamedVar),
        Sampled(SampledVar),
    }
    impl Var {
        pub fn new_bdd(
            id: UniqueId,
            label: VarLabel,
            provenance: Option<NamedVar>,
            fid: Option<FnId>,
        ) -> Self {
            Var::Bdd(BddVar::new(id, label, provenance, fid))
        }
        pub fn new_named(id: UniqueId, name: String) -> Self {
            Var::Named(NamedVar::new(id, name))
        }
        pub fn new_sampled(id: UniqueId, dist: SDist, provenance: Option<NamedVar>) -> Self {
            Var::Sampled(SampledVar::new(id, dist, provenance))
        }
        pub fn debug_id(&self) -> String {
            match self {
                Var::Bdd(v) => format!("{}", v.id),
                Var::Named(v) => v.name.clone(),
                Var::Sampled(v) => format!("{}", v.id),
            }
        }
        pub fn unsafe_label(&self) -> VarLabel {
            match self {
                Var::Bdd(v) => v.label,
                _ => panic!("shame on you!"),
            }
        }
        pub fn id(&self) -> UniqueId {
            match self {
                Var::Bdd(v) => v.id,
                Var::Named(v) => v.id,
                Var::Sampled(v) => v.id,
            }
        }
    }

    ::ttg::phase!(pub struct Annotated: {
        AVarExt<EVal>: NamedVar,
        AVarExt<SVal>: NamedVar,
     // APrjExt<EVal>: NamedVar,
     // APrjExt<SVal>: NamedVar,
        ADistExt<SVal>: SampledVar,

        ELetInExt: NamedVar,
        EFlipExt: BddVar,
        EAppExt: FnCall,
        EIterateExt: FnId,

        SLetInExt: NamedVar,
        SMapExt: NamedVar,
        SFoldExt: (NamedVar, NamedVar),
        SLambdaExt: Vec<NamedVar>,
        SAppExt: FnCall,
    });

    ::ttg::alias!(Annotated as Ann + (Program, EExpr, SExpr, Anf<Var>));
}

pub enum Fun {
    Exact(Function<EExpr<Annotated>>),
    Sample(Function<SExpr<Annotated>>),
}
impl Fun {
    fn err<X>(fr: &str, to: &str) -> Result<X> {
        errors::generic("tried to run a {fr} function in the {to} language")
    }
    pub fn exact(&self) -> Result<Function<EExpr<Annotated>>> {
        match self {
            Self::Exact(f) => Ok(f.clone()),
            Self::Sample(f) => Fun::err("sample", "exact"),
        }
    }
    pub fn sample(&self) -> Result<Function<SExpr<Annotated>>> {
        match self {
            Self::Exact(f) => Fun::err("exact", "sample"),
            Self::Sample(f) => Ok(f.clone()),
        }
    }
}

pub struct LabelEnv {
    pub lblsym: u64,
    pub subst_var: HashMap<UniqueId, Var>,
    pub letpos: Option<NamedVar>,
    pub fnctx: Option<FnId>,
    pub fids: HashMap<String, FnId>,
    pub fun_stats: HashMap<FnId, FnCounts>,
    pub funs: HashMap<FnId, Fun>,
    pub does_sample: bool,
}

pub fn insert_inv<T: Hash + Eq + Clone>(inv: &mut InvMap<T>, v: &T, provenance: &Option<NamedVar>) {
    match provenance {
        None => (),
        Some(prov) => match inv.get_mut(prov) {
            None => {
                let mut hs = HashSet::default();
                hs.insert(v.clone());
                inv.insert(prov.clone(), hs);
            }
            Some(vs) => {
                vs.insert(v.clone());
            }
        },
    }
}
fn annotate_anf_binop<VarExt, Val, DExt>(
    env: &mut LabelEnv,
    annotate_anf: impl Fn(&mut LabelEnv, &AnfUnq<Val>) -> Result<AnfAnn<Val>>,
    l: &AnfUnq<Val>,
    r: &AnfUnq<Val>,
    op: impl Fn(Box<AnfAnn<Val>>, Box<AnfAnn<Val>>) -> AnfAnn<Val>,
) -> Result<AnfAnn<Val>>
where
    AVarExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = VarExt>,
    // APrjExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = VarExt>,
    AValExt<Val>: ξ<Uniquify, Ext = ()> + ξ<Annotated, Ext = ()>,
    ADistExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = DExt>,
    Val: Debug + PartialEq + Clone,
    VarExt: Debug + PartialEq + Clone,
    DExt: Debug + PartialEq + Clone,
{
    Ok(op(
        Box::new(annotate_anf(env, l)?),
        Box::new(annotate_anf(env, r)?),
    ))
}
fn annotate_anf_vec<VarExt, Val, DExt>(
    env: &mut LabelEnv,
    annotate_anf: impl Fn(&mut LabelEnv, &AnfUnq<Val>) -> Result<AnfAnn<Val>>,
    xs: &[AnfUnq<Val>],
    op: impl Fn(Vec<AnfAnn<Val>>) -> AnfAnn<Val>,
) -> Result<AnfAnn<Val>>
where
    AVarExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = VarExt>,
    // APrjExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = VarExt>,
    AValExt<Val>: ξ<Uniquify, Ext = ()> + ξ<Annotated, Ext = ()>,
    ADistExt<Val>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = DExt>,
    Val: Debug + PartialEq + Clone,
    VarExt: Debug + PartialEq + Clone,
    DExt: Debug + PartialEq + Clone,
{
    Ok(op(xs
        .iter()
        .map(|a| annotate_anf(env, a))
        .collect::<Result<Vec<AnfAnn<Val>>>>()?))
}
impl LabelEnv {
    pub fn new(fids: HashMap<String, FnId>, fun_stats: HashMap<FnId, FnCounts>) -> Self {
        Self {
            lblsym: 0,
            subst_var: HashMap::default(),
            letpos: None,
            fnctx: None,
            fids,
            fun_stats: fun_stats
                .iter()
                .map(|(k, v)| {
                    (
                        *k,
                        FnCounts {
                            num_calls: v.num_calls,
                            num_uids: 0, // restart this counter
                            iterates: v.iterates,
                        },
                    )
                })
                .collect(),
            funs: HashMap::default(),
            does_sample: false,
        }
    }
    pub fn max_varlabel_val(&self) -> MaxVarLabel {
        MaxVarLabel(self.lblsym * 10)
    }

    pub fn linear_var_order(&self) -> rsdd::repr::var_order::VarOrder {
        rsdd::repr::var_order::VarOrder::linear_order(self.max_varlabel_val().0 as usize)
    }

    pub fn get_bdd_inv(&self) -> InvMap<BddVar> {
        let mut inv: InvMap<BddVar> = HashMap::default();
        for (_, var) in self.subst_var.iter() {
            match var {
                Var::Named(_) => continue,
                Var::Bdd(v) => insert_inv(&mut inv, v, &v.provenance),
                Var::Sampled(_) => continue,
            }
        }
        inv
    }
    pub fn get_sampled_inv(&self) -> InvMap<SampledVar> {
        let mut inv: InvMap<SampledVar> = HashMap::default();
        for (_, var) in self.subst_var.iter() {
            match var {
                Var::Named(_) => continue,
                Var::Bdd(_) => continue,
                Var::Sampled(v) => insert_inv(&mut inv, v, &v.provenance),
            }
        }
        inv
    }

    fn fresh(&mut self) -> VarLabel {
        let sym = self.lblsym;
        self.lblsym += 1;
        VarLabel::new(sym)
    }

    pub fn get_var(&self, id: &UniqueId) -> Result<Var> {
        match self.subst_var.get(id) {
            None => Err(CompileError::Generic(format!("symbol {id} not in scope"))),
            Some(x) => Ok(x.clone()),
        }
    }

    pub fn get_function_arg(&self, id: &UniqueId) -> Result<Var> {
        match self.subst_var.get(id) {
            None => Err(CompileError::Generic(format!("symbol {id} not in scope"))),
            Some(x) => Ok(x.clone()),
        }
    }

    fn annotate_eanf_binop(
        &mut self,
        l: &AnfUnq<EVal>,
        r: &AnfUnq<EVal>,
        op: impl Fn(Box<AnfAnn<EVal>>, Box<AnfAnn<EVal>>) -> AnfAnn<EVal>,
    ) -> Result<AnfAnn<EVal>> {
        Ok(op(
            Box::new(self.annotate_eanf(l)?),
            Box::new(self.annotate_eanf(r)?),
        ))
    }
    pub fn annotate_sanf(&mut self, a: &AnfUnq<SVal>) -> Result<AnfAnn<SVal>> {
        use crate::grammar::Anf::*;
        match a {
            AVar(uid, s) => {
                let var = self.get_var(uid)?;
                match var {
                    Var::Named(nvar) => Ok(AVar(nvar, s.to_string())),
                    Var::Bdd(_) => panic!("bdd vars are never referenced by source code!"),
                    Var::Sampled(_) => panic!("sampled vars are never referenced by source code!"),
                }
            }
            AVal(_, b) => Ok(AVal((), b.clone())),
            And(bl, br) => Ok(And(
                Box::new(self.annotate_sanf(bl)?),
                Box::new(self.annotate_sanf(br)?),
            )),
            Or(bl, br) => Ok(Or(
                Box::new(self.annotate_sanf(bl)?),
                Box::new(self.annotate_sanf(br)?),
            )),
            Xor(bl, br) => Ok(Xor(
                Box::new(self.annotate_sanf(bl)?),
                Box::new(self.annotate_sanf(br)?),
            )),
            Neg(bl) => Ok(Neg(Box::new(self.annotate_sanf(bl)?))),

            // Numerics
            Plus(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, Plus),
            Minus(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, Minus),
            Mult(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, Mult),
            Div(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, Div),

            // Ord
            GT(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, GT),
            LT(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, LT),
            GTE(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, GTE),
            LTE(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, LTE),
            EQ(l, r) => annotate_anf_binop(self, Self::annotate_sanf, l, r, EQ),

            // [x]; (l,r); x[0]
            AnfVec(xs) => annotate_anf_vec(self, Self::annotate_sanf, xs, AnfVec),
            AnfPush(xs, x) => Ok(AnfPush(
                Box::new(self.annotate_sanf(xs)?),
                Box::new(self.annotate_sanf(x)?),
            )),
            AnfHead(xs) => Ok(AnfHead(Box::new(self.annotate_sanf(xs)?))),
            AnfTail(xs) => Ok(AnfTail(Box::new(self.annotate_sanf(xs)?))),

            AnfProd(xs) => annotate_anf_vec(self, Self::annotate_sanf, xs, AnfProd),
            AnfPrj(var, ix) => Ok(AnfPrj(
                Box::new(self.annotate_sanf(var)?),
                Box::new(self.annotate_sanf(ix)?),
            )),
            AnfTrace(tr, x) => Ok(AnfTrace(
                Box::new(self.annotate_sanf(tr)?),
                Box::new(self.annotate_sanf(x)?),
            )),
            // Distributions
            AnfBernoulli(id, x) => {
                let var = SampledVar::new(*id, SDist::SBernoulli, self.letpos.clone());
                self.subst_var.insert(*id, Var::Sampled(var.clone()));
                Ok(AnfBernoulli(var, Box::new(self.annotate_sanf(x)?)))
            }
            AnfPoisson(id, x) => {
                let var = SampledVar::new(*id, SDist::SPoisson, self.letpos.clone());
                self.subst_var.insert(*id, Var::Sampled(var.clone()));
                Ok(AnfPoisson(var, Box::new(self.annotate_sanf(x)?)))
            }
            AnfUniform(id, l, r) => {
                let var = SampledVar::new(*id, SDist::SUniform, self.letpos.clone());
                self.subst_var.insert(*id, Var::Sampled(var.clone()));
                let l = Box::new(self.annotate_sanf(l)?);
                let r = Box::new(self.annotate_sanf(r)?);
                Ok(AnfUniform(var, l, r))
            }
            AnfNormal(id, l, r) => {
                let var = SampledVar::new(*id, SDist::SNormal, self.letpos.clone());
                self.subst_var.insert(*id, Var::Sampled(var.clone()));
                let l = Box::new(self.annotate_sanf(l)?);
                let r = Box::new(self.annotate_sanf(r)?);
                Ok(AnfNormal(var, l, r))
            }
            AnfBeta(id, l, r) => {
                let var = SampledVar::new(*id, SDist::SBeta, self.letpos.clone());
                self.subst_var.insert(*id, Var::Sampled(var.clone()));
                let l = Box::new(self.annotate_sanf(l)?);
                let r = Box::new(self.annotate_sanf(r)?);
                Ok(AnfBeta(var, l, r))
            }
            AnfDiscrete(id, xs) => {
                let var = SampledVar::new(*id, SDist::SDiscrete(xs.len()), self.letpos.clone());
                self.subst_var.insert(*id, Var::Sampled(var.clone()));
                let xs = xs
                    .iter()
                    .map(|a| self.annotate_sanf(a))
                    .collect::<Result<Vec<AnfAnn<SVal>>>>()?;
                Ok(AnfDiscrete(var, xs))
            }
            AnfDirichlet(id, xs) => {
                let var = SampledVar::new(*id, SDist::SDirichlet(xs.len()), self.letpos.clone());
                self.subst_var.insert(*id, Var::Sampled(var.clone()));
                let xs = xs
                    .iter()
                    .map(|a| self.annotate_sanf(a))
                    .collect::<Result<Vec<AnfAnn<SVal>>>>()?;
                Ok(AnfDirichlet(var, xs))
            }
        }
    }
    pub fn annotate_eanf(&mut self, a: &AnfUnq<EVal>) -> Result<AnfAnn<EVal>> {
        use crate::grammar::Anf::*;
        match a {
            AVar(uid, s) => {
                let var = self.get_var(uid)?;
                match var {
                    Var::Named(nvar) => Ok(AVar(nvar, s.to_string())),
                    Var::Bdd(_) => panic!("bdd vars are never referenced by source code!"),
                    Var::Sampled(_) => panic!("sampled vars are never referenced by source code!"),
                }
            }
            AVal(_, b) => Ok(AVal((), b.clone())),
            And(bl, br) => Ok(And(
                Box::new(self.annotate_eanf(bl)?),
                Box::new(self.annotate_eanf(br)?),
            )),
            Or(bl, br) => Ok(Or(
                Box::new(self.annotate_eanf(bl)?),
                Box::new(self.annotate_eanf(br)?),
            )),
            Xor(bl, br) => Ok(Xor(
                Box::new(self.annotate_eanf(bl)?),
                Box::new(self.annotate_eanf(br)?),
            )),
            Neg(bl) => Ok(Neg(Box::new(self.annotate_eanf(bl)?))),

            // Numerics
            Plus(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, Plus),
            Minus(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, Minus),
            Mult(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, Mult),
            Div(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, Div),

            // Ord
            GT(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, GT),
            LT(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, LT),
            GTE(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, GTE),
            LTE(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, LTE),
            EQ(l, r) => annotate_anf_binop(self, Self::annotate_eanf, l, r, EQ),

            AnfProd(xs) => annotate_anf_vec(self, Self::annotate_eanf, xs, AnfProd),
            AnfPrj(var, ix) => Ok(AnfPrj(
                Box::new(self.annotate_eanf(var)?),
                Box::new(self.annotate_eanf(ix)?),
            )),
            AnfTrace(tr, x) => Ok(AnfTrace(
                Box::new(self.annotate_eanf(tr)?),
                Box::new(self.annotate_eanf(x)?),
            )),

            // Distributions
            _ => errors::not_in_exact(),
        }
    }
    // we need to seed arguments as valid variables.
    pub fn annotate_arg<X: Debug + PartialEq + Clone, DExt: Debug + PartialEq + Clone>(
        &mut self,
        a: &AnfUnq<X>,
    ) -> Result<AnfAnn<X>>
    where
        AVarExt<X>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = NamedVar>,
        AValExt<X>: ξ<Uniquify, Ext = ()> + ξ<Annotated, Ext = ()>,
        // APrjExt<X>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = ()>,
        ADistExt<X>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = DExt>,
    {
        use crate::grammar::Anf::*;
        match a {
            AVar(id, s) => {
                let nvar = NamedVar::new(*id, s.to_string());
                let var = Var::Named(nvar.clone());
                self.subst_var.insert(*id, var);
                Ok(AVar(nvar, s.to_string()))
            }
            _ => errors::generic("only run annotate_args on variables"),
        }
    }
    pub fn annotate_args<X: Debug + PartialEq + Clone, DExt: Debug + PartialEq + Clone>(
        &mut self,
        anfs: &[AnfUnq<X>],
    ) -> Result<Vec<AnfAnn<X>>>
    where
        AVarExt<X>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = NamedVar>,
        AValExt<X>: ξ<Uniquify, Ext = ()> + ξ<Annotated, Ext = ()>,
        // APrjExt<X>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = ()>,
        ADistExt<X>: ξ<Uniquify, Ext = UniqueId> + ξ<Annotated, Ext = DExt>,
    {
        anfs.iter().map(|a| self.annotate_arg(a)).collect()
    }
    pub fn annotate_eanfs(&mut self, anfs: &[AnfUnq<EVal>]) -> Result<Vec<AnfAnn<EVal>>> {
        anfs.iter().map(|a| self.annotate_eanf(a)).collect()
    }
    pub fn annotate_sanfs(&mut self, anfs: &[AnfUnq<SVal>]) -> Result<Vec<AnfAnn<SVal>>> {
        anfs.iter().map(|a| self.annotate_sanf(a)).collect()
    }
    pub fn annotate_eexpr(&mut self, e: &EExprUnq) -> Result<EExprAnn> {
        use crate::grammar::EExpr::*;
        match e {
            EAnf(_, a) => Ok(EAnf((), Box::new(self.annotate_eanf(a)?))),
            // EPrj(_, i, a) => Ok(EPrj(
            //     (),
            //     Box::new(self.annotate_eanf(i)?),
            //     Box::new(self.annotate_eanf(a)?),
            // )),
            // EProd(_, anfs) => Ok(EProd((), self.annotate_eanfs(anfs)?)),
            ELetIn(id, s, ebound, ebody) => {
                let nvar = NamedVar::new(*id, s.to_string());
                let var = Var::Named(nvar.clone());
                self.letpos = Some(nvar.clone());
                // self.weights.insert(var.clone(), Weight::constant());
                self.subst_var.insert(*id, var);
                Ok(ELetIn(
                    nvar,
                    s.clone(),
                    Box::new(self.annotate_eexpr(ebound)?),
                    Box::new(self.annotate_eexpr(ebody)?),
                ))
            }
            EIte(_, cond, t, f) => Ok(EIte(
                (),
                Box::new(self.annotate_eanf(cond)?),
                Box::new(self.annotate_eexpr(t)?),
                Box::new(self.annotate_eexpr(f)?),
            )),
            EFlip(id, param) => {
                let lbl = self.fresh();
                let var = BddVar::new(*id, lbl, self.letpos.clone(), self.fnctx);
                self.subst_var.insert(*id, Var::Bdd(var.clone()));
                Ok(EFlip(var, Box::new(self.annotate_eanf(param)?)))
            }
            EIterate(i, f, init, k) => {
                let init = self.annotate_eanf(init)?;
                let k = self.annotate_eanf(k)?;
                Ok(EIterate(*i, f.clone(), Box::new(init), Box::new(k)))
            }

            EApp(i, f, args) => {
                let args = args
                    .iter()
                    .map(|a| self.annotate_eanf(a))
                    .collect::<Result<Vec<AnfAnn<EVal>>>>()?;
                Ok(EApp(*i, f.clone(), args))
            }
            EObserve(_, a, rst) => Ok(EObserve(
                (),
                Box::new(self.annotate_eanf(a)?),
                Box::new(self.annotate_eexpr(rst)?),
            )),
            ESample(_, e) => Ok(ESample((), Box::new(self.annotate_sexpr(e)?))),
            EDiscrete(_, _) => errors::erased(Annotated, "discrete"),
        }
    }
    pub fn annotate_sexpr(&mut self, e: &SExprUnq) -> Result<SExprAnn> {
        self.does_sample = true;
        use crate::grammar::SExpr::*;
        match e {
            SAnf(_, a) => Ok(SAnf((), Box::new(self.annotate_sanf(a)?))),
            SSeq(_, e0, e1) => Ok(SSeq(
                (),
                Box::new(self.annotate_sexpr(e0)?),
                Box::new(self.annotate_sexpr(e1)?),
            )),
            SLetIn(id, s, ebound, ebody) => {
                let nvar = NamedVar::new(*id, s.to_string());
                let var = Var::Named(nvar.clone());
                self.letpos = Some(nvar.clone());
                self.subst_var.insert(*id, var);
                Ok(SLetIn(
                    nvar,
                    s.clone(),
                    Box::new(self.annotate_sexpr(ebound)?),
                    Box::new(self.annotate_sexpr(ebody)?),
                ))
            }
            SIte(_, cond, t, f) => Ok(SIte(
                (),
                Box::new(self.annotate_sanf(cond)?),
                Box::new(self.annotate_sexpr(t)?),
                Box::new(self.annotate_sexpr(f)?),
            )),
            SObserve(_, a, e, rst) => Ok(SObserve(
                (),
                Box::new(self.annotate_sanf(a)?),
                Box::new(self.annotate_sanf(e)?),
                Box::new(self.annotate_sexpr(rst)?),
            )),
            SMap(arg_id, arg, body, xs) => {
                let nvar = NamedVar::new(*arg_id, arg.to_string());
                let var = Var::Named(nvar.clone());
                self.subst_var.insert(*arg_id, var);
                Ok(SMap(
                    nvar,
                    arg.clone(),
                    Box::new(self.annotate_sexpr(body)?),
                    Box::new(self.annotate_sanf(xs)?),
                ))
            }
            SFold((acc_id, arg_id), init, acc, arg, body, xs) => {
                let acc_nvar = NamedVar::new(*acc_id, acc.to_string());
                let acc_var = Var::Named(acc_nvar.clone());
                self.subst_var.insert(*acc_id, acc_var);
                let arg_nvar = NamedVar::new(*arg_id, arg.to_string());
                let arg_var = Var::Named(arg_nvar.clone());
                self.subst_var.insert(*arg_id, arg_var);

                Ok(SFold(
                    (acc_nvar, arg_nvar),
                    Box::new(self.annotate_sanf(init)?),
                    acc.clone(),
                    arg.clone(),
                    Box::new(self.annotate_sexpr(body)?),
                    Box::new(self.annotate_sanf(xs)?),
                ))
            }
            SWhile(_, guard, body) => Ok(SWhile(
                (),
                Box::new(self.annotate_sanf(guard)?),
                Box::new(self.annotate_sexpr(body)?),
            )),
            SApp(i, f, args) => Ok(SApp(*i, f.clone(), self.annotate_sanfs(args)?)),
            SLambda(ids, args, body) => {
                let nvars: Vec<NamedVar> = args
                    .iter()
                    .zip(ids.iter())
                    .map(|(a, id)| NamedVar::new(*id, a.to_string()))
                    .collect();
                Ok(SLambda(
                    nvars,
                    args.clone(),
                    Box::new(self.annotate_sexpr(body)?),
                ))
            }
            SSample(_, dist) => Ok(SSample((), Box::new(self.annotate_sexpr(dist)?))),

            SExact(_, e) => Ok(SExact((), Box::new(self.annotate_eexpr(e)?))),
            SLetSample(_, _, _, _) => errors::erased(Annotated, "let-sample"),
        }
    }
    pub fn annotate_efun(&mut self, f: &Function<EExprUnq>) -> Result<Function<EExprAnn>> {
        let name = f.name.clone();
        let arguments = f
            .arguments
            .iter()
            .map(|a| self.annotate_arg(a))
            .collect::<Result<Vec<_>>>()?;
        let body = self.annotate_eexpr(&f.body)?;
        let returnty = f.returnty.clone();
        Ok(Function {
            name,
            arguments,
            body,
            returnty,
        })
    }
    pub fn annotate_sfun(&mut self, f: &Function<SExprUnq>) -> Result<Function<SExprAnn>> {
        let name = f.name.clone();
        let arguments = f
            .arguments
            .iter()
            .map(|a| self.annotate_arg(a))
            .collect::<Result<Vec<_>>>()?;
        let body = self.annotate_sexpr(&f.body)?;
        let returnty = f.returnty.clone();
        Ok(Function {
            name,
            arguments,
            body,
            returnty,
        })
    }

    pub fn annotate_with_data(&mut self, p: &ProgramUnq, ds: &DataView) -> Result<AnnotateResult> {
        for (s, _, id) in &ds.keys {
            match id {
                Some(id) => {
                    let nvar = NamedVar::new(*id, s.to_string());
                    let var = Var::Named(nvar.clone());
                    self.subst_var.insert(*id, var.clone());
                }
                None => panic!("should be impossible..."),
            }
        }
        self.annotate(p)
    }

    #[allow(clippy::type_complexity)]
    pub fn annotate(&mut self, p: &ProgramUnq) -> Result<AnnotateResult> {
        match p {
            Program::SBody(e) => {
                self.fnctx = None;
                let start_bdd = self.max_varlabel_val();
                let sann = self.annotate_sexpr(e)?;
                let end_bdd = self.max_varlabel_val();
                let order = self.linear_var_order(); // FIXME this is the _worst_ order!!!
                let new_bdds = end_bdd.0 - start_bdd.0;
                let mx = compute_max_varlabel(start_bdd, &self.fun_stats, new_bdds);

                Ok(AnnotateResult::new(
                    Program::SBody(sann),
                    order,
                    self.subst_var.clone(),
                    MaxVarLabel(mx),
                    QueryType::Sample,
                    true,
                ))
            }
            Program::EBody(e) => {
                self.fnctx = None;
                let start_bdd = self.max_varlabel_val();
                let eann = self.annotate_eexpr(e)?;
                let end_bdd = self.max_varlabel_val();
                let order = self.linear_var_order(); // FIXME this is the _worst_ order!!!
                let new_bdds = end_bdd.0 - start_bdd.0;
                let mx = compute_max_varlabel(start_bdd, &self.fun_stats, new_bdds);
                Ok(AnnotateResult::new(
                    Program::EBody(eann),
                    order,
                    self.subst_var.clone(),
                    MaxVarLabel(mx),
                    QueryType::Exact,
                    self.does_sample,
                ))
            }
            Program::EDefine(f, p) => {
                let i = *self
                    .fids
                    .get(&f.name.clone().expect("name defined"))
                    .expect("function id created in previous pass");
                self.fnctx = Some(i);
                let start_bdds = self.max_varlabel_val();
                let f = self.annotate_efun(f)?;
                self.funs.insert(i, Fun::Exact(f.clone()));
                let end_bdds = self.max_varlabel_val();
                let ctr = self.fun_stats.get_mut(&i).expect("it's 9L up there!");
                ctr.num_uids = end_bdds.0 - start_bdds.0;
                // reserve the next #fncalls-worth of bdd ptrs
                self.lblsym += compute_function_block(ctr);
                let r = self.annotate(p)?;
                Ok(AnnotateResult::new(
                    Program::EDefine(f, Box::new(r.program)),
                    r.order,
                    r.idmap,
                    r.maxbdd,
                    r.query_type,
                    r.does_sample,
                ))
            }
            Program::SDefine(f, p) => {
                let i = *self
                    .fids
                    .get(&f.name.clone().expect("name defined"))
                    .expect("function id created in previous pass");
                self.fnctx = Some(i);
                let start_bdds = self.max_varlabel_val();
                let f = self.annotate_sfun(f)?;
                self.funs.insert(i, Fun::Sample(f.clone()));
                let end_bdds = self.max_varlabel_val();
                let ctr = self.fun_stats.get_mut(&i).expect("it's 9L up there!");
                ctr.num_uids = end_bdds.0 - start_bdds.0;
                // reserve the next #fncalls-worth of bdd ptrs
                self.lblsym += compute_function_block(ctr);

                let r = self.annotate(p)?;
                Ok(AnnotateResult::new(
                    Program::SDefine(f, Box::new(r.program)),
                    r.order,
                    r.idmap,
                    r.maxbdd,
                    r.query_type,
                    r.does_sample,
                ))
            }
        }
    }
}

fn compute_max_varlabel(
    start_bdd: MaxVarLabel,
    fun_stats: &HashMap<FnId, FnCounts>,
    new_bdds: u64,
) -> u64 {
    let MaxVarLabel(start_bdd) = start_bdd;
    let function_block_sizes = fun_stats
        .values()
        .map(|ctr| (ctr.num_calls + 1) * ctr.num_uids)
        .sum::<u64>();
    start_bdd + new_bdds + function_block_sizes
}
fn heuristic_bound() -> u64 {
    150
}
fn compute_function_block(ctr: &FnCounts) -> u64 {
    ctr.num_uids * ctr.num_calls * heuristic_bound()
}
pub fn pipeline(p: &ProgramInferable) -> Result<AnnotateResult> {
    let p = crate::uniquify::pipeline(p)?;
    let mut lenv = LabelEnv::new(p.1.functions, p.1.fun_stats);
    lenv.annotate(&p.0)
}
