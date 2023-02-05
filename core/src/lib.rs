use std::collections::HashMap;
use std::string::String;

#[derive(Debug)]
struct Env {
    vars: HashMap<String, Vec<u8>>,
    gen: u8,
    // rgen : StdGen,
}

// -- unified identity for variables and the weight map
// def mkFresh [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] (os : Option String) : m Nat :=
//   let getter := do
//     let env <- get
//     let n := env.gen
//     let ns := match os with | none => s!"_{n}" | some x => s!"{x}"
//     set { env with gen := n+1, vars := env.vars.insert ns [n] }
//     let oenv := env
//     let env <- get
//     -- dbg_trace "[mkFresh.getter]  ( {HashMap.toList oenv.vars} + {oenv.gen} ) {os}->{n} ( {HashMap.toList env.vars} + {env.gen} )";
//     return n
//   match os with
//   -- A little hack to work around unnamed vars missing from context.
//   | none => getter
//   | some x => do
//     let env <- get
//     match env.vars.find? x with
//     | none => getter
//     | some _ => MonadExceptOf.throw s!"{x} already in environment"

// def fresh [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] : m Nat := mkFresh none

// def getVar? [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] (x: String) : m (Option Nat) := do
//   let env <- get
//   match env.vars.find? x with
//   | none => pure none
//   | some [v] => pure (some v)
//   | some vs => MonadExceptOf.throw s!"multiple ids found for {x}: {vs}"

// -- hacks on hacks, just want this out the door
// def _getVar?? (p : SubstMap) (n: Nat) : Nat -> Nat
//   | Nat.zero => n
//   | Nat.succ lvl =>
//     match p.find? n with
//     | none => n
//     | some (Formula.id n) => _getVar?? p n lvl
//     | some _ => n

// -- getVar? but also say if it was in the environment
// def getVar?? [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] (x: String) (p : SubstMap) : m (Nat × Bool) := do
//   let ov <- getVar? x
//   match ov with
//   | none => do
//     let v <- mkFresh (some x)
//     pure (v, false)
//   | some v => do
//     let vv := _getVar?? p v 20
//     -- dbg_trace "[getVar??] {v}->{vv}"
//     pure (vv, true)

// inductive Ty where
//   | bool : Ty
//   | prod (l : Ty) (r : Ty) : Ty
// deriving Repr, BEq

// inductive Val where
//   | bool (b:Bool) : Val
//   | prod (l : Val) (r : Val) : Val
// deriving Repr, BEq

// inductive ANF where
//   | var (s:String) : ANF
//   | val (v:Val) : ANF

//   -- I don't think I /should/ do this, but it does add some nice sugar...
//   | and (l:ANF) (r:ANF) : ANF
//   | or  (l:ANF) (r:ANF) : ANF
//   | neg (l:ANF) : ANF
// deriving Repr, BEq

// inductive Expr where
//   | anf (a:ANF)
//   | fst (a:ANF)
//   | snd (a:ANF)
//   | prod (l:ANF) (r:ANF)
//   | letIn (x:String) (e:Expr) (rest:Expr)
//   | ite (a:ANF) (t:Expr) (f:Expr)
//   -- | app (f:String) (a:ANF) -- FIXME: later
//   | flip (p:Float)
//   | observe (a:ANF) -- TODO this can be a full expression, I think
//   | sample (e:Expr)
// deriving Repr, BEq

// def Expr.avar (s:String) := Expr.anf (ANF.var s)
// def Expr.atrue := Expr.anf (ANF.val (Val.bool true))
// def Expr.afalse := Expr.anf (ANF.val (Val.bool false))
// def Expr.and (l:ANF) (r:ANF) := Expr.anf (ANF.and l r)
// def Expr.or  (l:ANF) (r:ANF) := Expr.anf (ANF.or l r)
// def Expr.neg (l:ANF) := Expr.anf (ANF.neg l)

// structure Func where
//   name : String
//   arg : String × Ty
//   ret : Ty
//   body : Expr
// deriving Repr

// inductive Program where
//   | body (body: Expr) : Program
//   | define (f: Func) (rest: Program) : Program
// deriving Repr

// def Formula.apply (f: Formula) (m: SubstMap) : Formula :=
//   match f with
//   | id x =>
//     match m.find? x with
//     | none => id x
//     | some x' => x'
//   | bool b => bool b
//   | disj l r => disj (l.apply m) (r.apply m)
//   | conj l r => conj (l.apply m) (r.apply m)
//   | neg b => neg (b.apply m)

// def Formula._max (cur: Nat) (f: Formula) : Nat :=
//   match f with
//   | id x => if cur > x then cur else x
//   | bool b => cur
//   | disj l r =>
//     let lmx := l._max cur
//     let rmx := r._max cur
//     if lmx > rmx then lmx else rmx
//   | conj l r =>
//     let lmx := l._max cur
//     let rmx := r._max cur
//     if lmx > rmx then lmx else rmx
//   | neg b => b._max cur

// def Formula.max (f: Formula) : Nat := f._max 0

// infixl:55 " \\/ " => Formula.disj
// infixl:55 " /\\ " => Formula.conj

// structure Prob where
//   prob : Float
// deriving Repr

// def Prob.add (l: Prob) (r:Prob) := Prob.mk (l.prob + r.prob)
// def Prob.mul (l: Prob) (r:Prob) := Prob.mk (l.prob * r.prob)

// def leftEntry {a b : Type} (_ : a) (left : b) (_ : b) : b := left

// structure Compiled where
//   global : Formula
//   accept : Formula
//   weightMap : WeightMap
//   substitutions : SubstMap
//   probability : Prob
//   importanceWeight : Float

// def Compiled.unionMaps (l: Compiled) (r:Compiled) := l.weightMap.mergeWith leftEntry r.weightMap
// def Compiled.unionSubs (l: Compiled) (r:Compiled) := l.substitutions.mergeWith leftEntry r.substitutions
// def Compiled.convexCombIW (l: Compiled) (r:Compiled) := l.probability.prob * l.importanceWeight + r.probability.prob * r.importanceWeight

// class Monoid (α : Type u) where
//   unit : α
//   op   : α → α → α

// instance : Monoid Compiled where
//   unit := { global := Formula.bool true, accept := Formula.bool true, weightMap := HashMap.empty, substitutions := HashMap.empty, probability := Prob.mk 1, importanceWeight := 1 }
//   op l r := { global := Formula.conj l.global r.global, accept := Formula.conj l.accept r.accept, weightMap := l.unionMaps r, substitutions := l.unionSubs r, probability := Prob.mul l.probability r.probability, importanceWeight := l.importanceWeight * r.importanceWeight }

// def map2vars (m: WeightMap) : HashSet Nat := Id.run do
//   let list := fst <$> m.toList
//   let mut fin : HashSet Nat := Lean.mkHashSet (capacity := m.size)
//   for n in list do
//      fin := HashSet.insert fin n
//   fin

// -- abbrev Assignment := HashMap Nat Bool
// abbrev Assignment := Array Bool

// structure AllAssignments where
//   cur  : Option Assignment
//   num_vars : Nat
// deriving Repr

// -- tired of trying to get HXor to synthesize
// def bXor : Bool -> Bool -> Bool
//   | false, true => true
//   | true, false  => true
//   | _, _  => false

// instance : Stream AllAssignments Assignment where
//   next? state :=
//     match state.cur with
//     | none => -- start a stream
//       let cur := Array.mk ((fun _ => false) <$> List.range state.num_vars)
//       some (cur, { state with cur := cur})
//     | some cur =>
//       let emptyBoolArr : Array Bool := Array.empty
//       let (nxt, carry) := Array.foldl (fun (cur_l, carry) cur_assgn =>
//         let new_itm := bXor cur_assgn carry
//         let new_carry := cur_assgn && carry
//         (cur_l.push new_itm, new_carry)
//         ) (emptyBoolArr, true) cur
//       if carry
//       then none
//       else some (nxt, { state with cur := nxt })

// -- #eval Stream.next? (AllAssignments.mk none 3)
// -- #eval Stream.next? (AllAssignments.mk #[true, true, false] 3)
// -- #eval Stream.next? (AllAssignments.mk #[true, true, true] 3)
// def all_assignments (n : Nat) : AllAssignments := { cur := none, num_vars := n }

// def isSAT [Monad m] [MonadExceptOf String m] (a: Assignment) : Formula -> m Bool
//   | .id i =>
//     match a[i]? with
//     | none =>
//       -- dbg_trace "{a}?{i}"
//       throw s!"id {i} was not foud in assignments list of length {a.size}!"
//     | some b => pure b
//   | .bool b => pure (b == true)
//   | .disj l r => (. || .) <$> isSAT a l <*> isSAT a r
//   | .conj l r => (. && .) <$> isSAT a l <*> isSAT a r
//   | .neg b => not <$> isSAT a b

// @[always_inline, inline] protected def Option.tryCatchStr (x : Option α) (handle : String → Option α) : Option α :=
//   match x with
//   | some _ => x
//   | none => handle ""

// instance : MonadExceptOf String Option where
//   throw    := fun _ => Option.none
//   tryCatch := Option.tryCatchStr

// def isConsecutive [Monad m] [MonadExceptOf String m] (nats: List Nat) : m Bool :=
//   match nats.maximum? with
//   | none => throw "error: empty weight map in validVars"
//   | some mx => pure $ nats.length - 1 == mx

// -- #eval (isConsecutive [] : Except String Bool)
// -- #eval (isConsecutive [2,1,3,0] : Except String Bool)

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

// def evalANF [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] : ANF -> WeightMap -> SubstMap -> m Compiled
// -- $\begin{prooftree}
// -- \hypo{\textrm{fresh } x^{\prime}}
// -- \infer1[IS-Id]{\langle x, m, \rho\rangle \Downarrow \langle x^{\prime}, \texttt{T}, m \cup [x^{\prime} \mapsto \frac{1}{2},\bar{x}^{\prime} \mapsto \frac{1}{2}], \rho, 1, 1 \rangle }
// -- \end{prooftree}$
//   | ANF.var s, m, p => do
//     let (v, inEnv) <- getVar?? s p
//     let m := if inEnv then m else m.insert v constWeight
//     let c := { (Monoid.unit : Compiled) with global := Formula.id v, weightMap := m, substitutions := p }
//     -- +dbg_trace "[evalANF][var {s}->{v}]";
//     -- +dbg_trace "[evalANF] ( ,  {HashMap.toList m}, {HashMap.toList p} )";
//     -- +dbg_trace "[evalANF]      \\||/  {c.global}";
//     -- +dbg_trace "[evalANF]      \\||/  {c.accept}";
//     -- +dbg_trace "[evalANF]      \\||/  {HashMap.toList c.weightMap}";
//     -- +dbg_trace "[evalANF]      \\||/  {HashMap.toList c.substitutions}";
//     -- +dbg_trace "[evalANF][var]";
//     return c

//   | ANF.val (Val.bool b), m, p   => return { (Monoid.unit : Compiled) with global := Formula.bool b, weightMap := m, substitutions := p }
//   | ANF.val (Val.prod l r), m, p => do
//     let l <- evalANF (ANF.val l) m p
//     let r <- evalANF (ANF.val r) m p
//     return Monoid.op l r
//   -- boolean operators:
//   | .neg a, m, p => do
//     let c <- evalANF a m p
//     return { c with global := Formula.neg c.global }
//   | .and l r, m, p => do
//     let l <- evalANF l m p
//     let r <- evalANF r m p
//     return { Monoid.op l r with global := fconj l.global r.global }
//   | .or l r, m, p => do
//     let l <- evalANF l m p
//     let r <- evalANF r m p
//     return { Monoid.op l r with global := Formula.disj l.global r.global }

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
