#![allow(dead_code)]
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
use std::collections::HashMap;
use std::string::String;

pub mod grammar {
    #[derive(Debug, Copy, Clone)]
    pub enum Ty {
        Bool,
        // Prod(Box<Ty>, Box<Ty>), // TODO for now, no tuples
    }
    #[derive(Debug, Copy, Clone)]
    pub enum Val {
        Bool(bool),
        // Prod(Box<Val>, Box<Val>), // TODO punt
    }
    #[derive(Debug, Clone)]
    pub enum ANF {
        AVar(String),
        AVal(Val),
        // TODO: not sure this is where I should add booleans, but it makes
        // the observe statements stay closer to the semantics: ~observe anf~
        And(Box<ANF>, Box<ANF>),
        Or(Box<ANF>, Box<ANF>),
        Neg(Box<ANF>),
    }
    #[derive(Debug, Clone)]
    pub enum Expr {
        EAnf(Box<ANF>),
        // TODO Ignore product types for now:
        // EFst (Box<ANF>),
        // ESnd (Box<ANF>),
        // EProd (Box<ANF>, Box<ANF>),

        // TODO Ignore function calls for now
        // EApp(String, Box<ANF>),
        ELetIn(String, Box<Expr>, Box<Expr>),
        EIte(Box<ANF>, Box<Expr>, Box<Expr>),
        EFlip(f32),
        EObserve(Box<ANF>),
        ESample(Box<Expr>),
    }
    // TODO
    // structure Func where
    //   name : String
    //   arg : String × Ty
    //   ret : Ty
    //   body : Expr
    // deriving Repr

    #[derive(Debug, Clone)]
    pub enum Program {
        Body(Expr),
        // TODO
        // | define (f: Func) (rest: Program) : Program
    }
}
pub mod semantics {
    use super::*;
    use grammar::*;
    use rsdd::builder::cache::*;
    use rsdd::repr::bdd::*;
    use rsdd::repr::ddnnf::*;
    use rsdd::repr::var_label::*;
    use rsdd::sample::probability::Probability;
    type WeightMap = HashMap<VarLabel, (f32, f32)>;
    fn const_weight() -> (f32, f32) {
        (0.5, 0.5)
    }
    type SubstMap = HashMap<VarLabel, BddPtr>;

    pub struct Compiled {
        dist: BddPtr,
        accept: BddPtr,
        weight_map: WeightMap, // must be a hashmap as sample will collapse variables
        substitutions: SubstMap,
        probability: Probability,
        importance_weight: f64,
    }
    impl Compiled {
        // FIXME: need to think about these semantics again.
        fn conj_extend<T: LruTable<BddPtr>>(&mut self, mgr: &mut BddManager<T>, o: Compiled) {
            self.dist = mgr.and(self.dist, o.dist);
            self.accept = mgr.and(self.accept, o.accept);
            self.weight_map.extend(o.weight_map);
            self.substitutions.extend(o.substitutions);
            self.probability = self.probability * o.probability;
            self.importance_weight *= o.importance_weight;
        }
        // FIXME: need to think about these semantics again.
        fn disj_extend<T: LruTable<BddPtr>>(&mut self, mgr: &mut BddManager<T>, o: Compiled) {
            self.dist = mgr.or(self.dist, o.dist);
            self.accept = mgr.or(self.accept, o.accept);
            self.weight_map.extend(o.weight_map);
            self.substitutions.extend(o.substitutions);
            self.probability = self.probability + o.probability;
            self.importance_weight = self.importance_weight * self.probability.as_f64()
                + o.importance_weight * o.probability.as_f64();
        }
    }

    impl Default for Compiled {
        fn default() -> Compiled {
            Compiled {
                dist: BddPtr::PtrTrue,
                accept: BddPtr::PtrTrue,
                weight_map: HashMap::new(),
                substitutions: HashMap::new(),
                probability: Probability::new(1.0),
                importance_weight: 1.0,
            }
        }
    }

    pub type Sym = u64;

    pub struct Env<'a> {
        vars: HashMap<String, Sym>,
        gensym: Sym,
        mgr: &'a mut BddManager<AllTable<BddPtr>>,
        // rgen : StdGen,
    }
    impl<'a> Env<'a> {
        fn _fresh(&mut self, ovar: Option<String>) -> Sym {
            let sym = self.gensym;
            self.gensym += 1;
            let var = ovar.unwrap_or(format!("_{sym}"));
            self.vars.insert(var, sym);
            sym
        }
        fn fresh(&mut self) -> Sym {
            self._fresh(None)
        }
        fn get_var(&self, var: String) -> Option<Sym> {
            self.vars.get(&var).copied()
        }
        fn get_or_create(&mut self, var: String) -> Sym {
            let osym = self.get_var(var.clone());
            osym.unwrap_or_else(|| self._fresh(Some(var)))
        }
        fn eval_anf(&mut self, a: ANF, m: &WeightMap, p: &SubstMap) -> Compiled {
            use ANF::*;
            match a {
                AVar(s) => {
                    let (lbl, wm) = match self.get_var(s) {
                        None => {
                            let sym = self.fresh();
                            let lbl = VarLabel::new(sym);
                            let m_ = &mut m.clone();
                            m_.insert(lbl, const_weight());
                            (lbl, m_.clone())
                        }
                        Some(sym) => {
                            let lbl = VarLabel::new(sym);
                            (lbl, m.clone())
                        }
                    };
                    Compiled {
                        substitutions: p.clone(),
                        dist: self.mgr.var(lbl, true),
                        weight_map: wm,
                        ..Default::default()
                    }
                }
                AVal(Val::Bool(b)) => Compiled {
                    dist: BddPtr::from_bool(b),
                    substitutions: p.clone(),
                    weight_map: m.clone(),
                    ..Default::default()
                },
                And(bl, br) => {
                    let mut l = self.eval_anf(*bl, m, p);
                    let r = self.eval_anf(*br, m, p);
                    l.conj_extend(self.mgr, r);
                    l
                }
                Or(bl, br) => {
                    let mut l = self.eval_anf(*bl, m, p);
                    let r = self.eval_anf(*br, m, p);
                    l.disj_extend(self.mgr, r);
                    l
                }
                Neg(bp) => {
                    let mut p = self.eval_anf(*bp, m, p);
                    p.dist = p.dist.neg();
                    p
                }
            }
        }
    }
    //   -- boolean operators:
    //   | .neg a, m, p => do
    //     let c <- evalANF a m p
    //     return { c with global := Formula.neg c.global }
    //   | .and l r, m, p => do
    //     return { Monoid.op l r with global := fconj l.global r.global }
    //   | .or l r, m, p => do
    //     let l <- evalANF l m p
    //     let r <- evalANF r m p
    //     return { Monoid.op l r with global := Formula.disj l.global r.global }
}

// def assertConsecutiveVars [Monad m] [MonadExceptOf String m] (weights: WeightMap) : m Unit := do
//   let num_vars := weights.size
//   let isValid <- isConsecutive <| HashSet.toList <| map2vars weights
//   if isValid then pure () else
//     let keys : List Nat := weights.toList.map fst
//     throw s!"weight keys are not consecutive nats. Expected keys: [0,{num_vars}). Got:{keys}"

// def wmc [Monad m] [MonadExceptOf String m] (env: Env) (weights: WeightMap) (f: Formula) : m Float := do
//   dbg_trace "[wmc] {HashMap.toList weights} {f}";
//   -- assertConsecutiveVars weights
//   let t := Id.run do
//     let num_vars := f.max + 1 -- env.vars.size -- weights.size
//     let init : Float := 0
//     Stream.fold (all_assignments num_vars) (fun tot assgn =>
//       match (isSAT assgn f : Except String Bool) with
//       | .error e =>
//         dbg_trace "[wmc]{assgn}: false -- got err: {e}";
//         tot
//       | .ok b =>
//         if not b
//         then
//           dbg_trace "[wmc]{assgn}: false";
//           pure tot
//         else
//           let w := Id.run do
//             let cur <- tot
//             let (_, diff) <- (Array.foldl (fun (ix, v) p =>
//               match weights.find? ix with
//               | none => (ix+1, v)
//               | some (l, h) => if l + h == 0 then (ix+1, v) else
//                 let w := if p then h else l
//                 -- -- dbg_trace "( {assgn} ) {p} ( {l}, {h} )";
//                 (ix+1, v * w)
//             ) ((0 : Nat), (1 : Float)) assgn)
//             dbg_trace "[wmc]{assgn}: true : {diff}";
//             return diff + cur
//           pure w
//     ) init
//   dbg_trace "[wmc] ...final: {t}";
//   pure t

// def bernoulli [Monad m] [MonadStateOf Env m] (theta : Prob) : m Bool := do
//   let env <- get
//   let mx := stdRange.snd
//   let (n, rgen) := randNat env.rgen 0 mx
//   set { env with rgen := rgen }
//   let asprob : Float := n.toFloat / mx.toFloat
//   let val : Bool := asprob > theta.prob
//   return val

// def constWeight := (0.0, 0.0)

// def dbg_compiled [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] (s:String) (w: WeightMap) (p: SubstMap) (c: Compiled) : m Unit := do
//     dbg_trace "[evalExpr][{s}]";
//     dbg_trace "[evalExpr] (_,  {HashMap.toList w}, {HashMap.toList p} )";
//     dbg_trace "[evalExpr]      \\||/  {c.global}";
//     dbg_trace "[evalExpr]      \\||/  {c.accept}";
//     dbg_trace "[evalExpr]      \\||/  {HashMap.toList c.weightMap}";
//     dbg_trace "[evalExpr]      \\||/  {HashMap.toList c.substitutions}";
//     dbg_trace "[evalExpr]      \\||/  {c.probability.prob}";
//     dbg_trace "[evalExpr]      \\||/  {c.importanceWeight}";
//     dbg_trace "[evalExpr][{s}]";
//     pure ()
// open Formula in
// def evalExpr [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] : Expr -> WeightMap -> SubstMap -> m Compiled
//   | Expr.anf a, m, p => evalANF a m p
//   | Expr.fst a, m, p => evalANF a m p
//   | Expr.snd a, m, p => evalANF a m p
//   | Expr.prod l r, m, p => do
//     let l <- evalANF l m p
//     let r <- evalANF r m p
//     return Monoid.op l r
//   | Expr.letIn s e rest, m, p => do
//     let foo <- getVar? s
//     let (x, inEnv) <- getVar?? s p
//     let m := if inEnv then m else m.insert x constWeight
//     let comp_1 <- evalExpr e m p
//     let comp_2 <- evalExpr rest comp_1.weightMap (comp_1.substitutions.insert x comp_1.global)
//     let c := {
//       global        := comp_2.global.subst x comp_1.global
//       accept        := fconj comp_1.accept (comp_2.accept.subst x comp_1.accept)
//       weightMap     := comp_1.unionMaps comp_2
//       substitutions := comp_1.unionSubs comp_2
//       probability   := comp_1.probability.mul comp_2.probability
//       importanceWeight := comp_1.importanceWeight * comp_2.importanceWeight
//       }
//     dbg_compiled "letIn({s}->{x})" m p c
//     dbg_trace "[evalExpr]      let {s}:{x} / {foo} in ...";
//     dbg_trace "[evalExpr]      global with substitution: {comp_2.global} -> {c.global}"
//     dbg_trace "[evalExpr][letIn]";
//     pure c

//   | Expr.ite pred t f, m, p => do
//     let a <- evalANF pred m p
//     let comp_t <- evalExpr t m p
//     let comp_f <- evalExpr f m p
//     return { global := (fconj a.global comp_t.global) \/ (neg a.global /\ comp_f.global)
//            , accept := (fconj a.accept comp_t.accept) \/ (neg a.accept /\ comp_f.accept)
//            , weightMap     := comp_t.unionMaps comp_f
//            , substitutions := comp_t.unionSubs comp_f
//            , probability   := comp_t.probability.add comp_f.probability
//            , importanceWeight := comp_t.convexCombIW comp_f
//            }
//   | Expr.flip param, m, p => do
//     let f <- fresh
//     let flipMap := HashMap.empty.insert f (1-param, param)
//     let c := {
//       global := Formula.id f
//       accept := Formula.bool true
//       weightMap     := flipMap.mergeWith leftEntry m
//       substitutions := p
//       probability   := Prob.mk 1
//       importanceWeight := 1
//       }
//     dbg_compiled "flip {f}" m p c
//     pure c
//   | Expr.observe a, m, p => do
//     let acomp <- evalANF a m p
//     let env <- get
//     -- let w <- wmc env m (acomp.global.apply p)
//     let a <- wmc env m (Formula.conj (acomp.global.apply p) (acomp.accept.apply p))
//     let z <- wmc env m (acomp.accept.apply p)
//     let w := a / z
//     let c := {
//       global := Formula.bool true
//       accept := acomp.global
//       weightMap     := m
//       substitutions := p
//       probability   := Prob.mk 1
//       importanceWeight := w
//       }
//     dbg_compiled "observe _" m p c
//     pure c
//   | Expr.sample e, m, p => do
//     let ecomp <- evalExpr e m p
//     if ecomp.accept != bool true then throw "sample statement includes observe statement" else
//     let env <- get
//     let theta_q <- wmc env ecomp.weightMap ecomp.global
//     let v <- bernoulli (Prob.mk theta_q)
//     let q := Prob.mk (if v then theta_q else 1-theta_q)

//     let c := {
//       global := Formula.bool v
//       accept := Formula.bool true -- FIXME(!!!) need to switch back to this when you are clear on part 1 of sample statements: ecomp.global
//       weightMap     := m
//       substitutions := p
//       probability   := q
//       importanceWeight := 1
//       }
//     dbg_compiled "sample {v}" m p c
//     pure c
