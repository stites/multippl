import Std.Data.HashMap
import Lean.Data.HashSet
import Std.Data.RBMap
import Init.Control.Id
import Init.Data.Option
open Std
open Lean (HashSet)
open Prod
-------------------------------------------------------------------------------------------------
-- flake.nix is giving me trouble, taking the following from https://github.com/Kha/aoc-2022/blob/master/Aoc/Util.lean
instance [Stream ρ α] : ToStream ρ ρ where
  toStream s := s

def Stream.fold [ToStream ρ ρ'] [Stream ρ' α] (s : ρ) (f : β → α → β) (init : β) : β := Id.run do
  let mut b := init
  for a in toStream s do
    b := f b a
  b
-------------------------------------------------------------------------------------------------

structure Env where
  vars : HashMap String (List Nat)
  gen : Nat
  rgen : StdGen

-- unified identity for variables and the weight map
def fresh [Monad m] [MonadStateOf Env m] : m Nat := do
  let env <- get
  set { env with gen := env.gen+1 }
  return env.gen

def getId [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] (x: String) : m Nat := do
  let env <- get
  match env.vars.find? x with
  | none => MonadExceptOf.throw s!"no variable {x} found"
  | some [v] => return v
  | some vs => MonadExceptOf.throw s!"multiple ids found: {vs}"

-- unified identity for variables and the weight map
def getFresh [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] (x: String) : m Nat := do
  let env <- get
  match env.vars.find? x with
  | none => do
    set { env with vars := env.vars.insert x [env.gen], gen := env.gen+1 }
    return env.gen
  | some _ => MonadExceptOf.throw s!"{x} already in environment"

inductive Ty where
  | bool : Ty
  | prod (l : Ty) (r : Ty) : Ty
deriving Repr, BEq

inductive Val where
  | bool (b:Bool) : Val
  | prod (l : Val) (r : Val) : Val
deriving Repr, BEq

inductive ANF where
  | var (s:String) : ANF
  | val (v:Val) : ANF
deriving Repr, BEq

inductive Expr where
  | anf (a:ANF)
  | fst (a:ANF)
  | snd (a:ANF)
  | prod (l:ANF) (r:ANF)
  | letIn (x:String) (e:Expr) (rest:Expr)
  | ite (a:ANF) (t:Expr) (f:Expr)
--  | app (f:String) (a:ANF) -- FIXME: later
  | flip (p:Float)
  | observe (a:ANF) -- TODO this can be a full expression, I think
  | sample (e:Expr)
deriving Repr, BEq

structure Func where
  name : String
  arg : Prod String Ty
  ret : Ty
  body : Expr
deriving Repr

inductive Program where
  | body (body: Expr) : Program
  | define (f: Func) (rest: Program) : Program
deriving Repr

inductive Formula where
  | id (x: Nat) : Formula
  | bool (b: Bool) : Formula
  | disj (l: Formula) (r: Formula) : Formula
  | conj (l: Formula) (r: Formula) : Formula
  | neg (b: Formula)  : Formula
deriving Repr, BEq

def Formula.subst (f: Formula) (x: Nat) (v: Formula) :=
  match f with
  | id _ => v
  | bool b => bool b
  | disj l r => disj (l.subst x v) (r.subst x v)
  | conj l r => conj (l.subst x v) (r.subst x v)
  | neg b => neg (b.subst x v)

abbrev WeightMap := HashMap Nat (Prod Float Float)
abbrev SubstMap := HashMap Nat Formula

def Formula.apply (f: Formula) (m: SubstMap) : Formula :=
  match f with
  | id x =>
    match m.find? x with
    | none => id x
    | some x' => x'
  | bool b => bool b
  | disj l r => disj (l.apply m) (r.apply m)
  | conj l r => conj (l.apply m) (r.apply m)
  | neg b => neg (b.apply m)


infixl:55 " \\/ " => Formula.disj
infixl:55 " /\\ " => Formula.conj

structure Prob where
  prob : Float
deriving Repr

def Prob.add (l: Prob) (r:Prob) := Prob.mk (l.prob + r.prob)
def Prob.mul (l: Prob) (r:Prob) := Prob.mk (l.prob * r.prob)

def leftEntry {a b : Type} (_ : a) (left : b) (_ : b) : b := left

structure Compiled where
  global : Formula
  accept : Formula
  weightMap : WeightMap
  substitutions : SubstMap
  probability : Prob
  importanceWeight : Float

-- could really use some lenses, I think
def Compiled.unionMaps (l: Compiled) (r:Compiled) := l.weightMap.mergeWith leftEntry r.weightMap
def Compiled.unionSubs (l: Compiled) (r:Compiled) := l.substitutions.mergeWith leftEntry r.substitutions
def Compiled.convexCombIW (l: Compiled) (r:Compiled) := l.probability.prob * l.importanceWeight + r.probability.prob * r.importanceWeight

class Monoid (α : Type u) where
  unit : α
  op   : α → α → α

instance : Monoid Compiled where
  unit := { global := Formula.bool true, accept := Formula.bool true, weightMap := HashMap.empty, substitutions := HashMap.empty, probability := Prob.mk 1, importanceWeight := 1 }
  op l r := { global := Formula.conj l.global r.global, accept := Formula.conj l.accept r.accept, weightMap := l.unionMaps r, substitutions := l.unionSubs r, probability := Prob.mul l.probability r.probability, importanceWeight := l.importanceWeight * r.importanceWeight }

def map2vars (m: WeightMap) : HashSet Nat := Id.run do
  let list := Prod.fst <$> m.toList
  let mut fin : HashSet Nat := Lean.mkHashSet (capacity := m.size)
  for n in list do
     fin := HashSet.insert fin n
  fin

abbrev Assignment := Array Bool

structure AllAssignments where
  cur  : Option Assignment
  num_vars : Nat
deriving Repr

-- tired of trying to get HXor to synthesize
def bXor : Bool -> Bool -> Bool
  | false, true => true
  | true, false  => true
  | _, _  => false

instance : Stream AllAssignments Assignment where
  next? state :=
    match state.cur with
    | none => -- start a stream
      let cur := Array.mk ((fun _ => false) <$> List.range state.num_vars)
      some (cur, { state with cur := cur})
    | some cur =>
      let emptyBoolArr : Array Bool := Array.empty
      let (nxt, carry) := Array.foldl (fun (cur_l, carry) cur_assgn =>
        let new_itm := bXor cur_assgn carry
        let new_carry := cur_assgn && carry
        (cur_l.push new_itm, new_carry)
        ) (emptyBoolArr, true) cur
      if carry
      then none
      else some (nxt, { state with cur := nxt })

#eval Stream.next? (AllAssignments.mk none 3)
#eval Stream.next? (AllAssignments.mk #[true, true, false] 3)
#eval Stream.next? (AllAssignments.mk #[true, true, true] 3)
def all_assignments (n : Nat) : AllAssignments := { cur := none, num_vars := n }

def isSAT [Monad m] [MonadExceptOf String m] (a: Assignment) : Formula -> m Bool
  | .id i =>
    match a[i]? with
    | none => throw s!"id {i} was not foud in assignments list of length {a.size}!"
    | some b => pure b
  | .bool b => return (b == true)
  | .disj l r => (. || .) <$> isSAT a l <*> isSAT a r
  | .conj l r => (. && .) <$> isSAT a l <*> isSAT a r
  | .neg b => not <$> isSAT a b

@[always_inline, inline] protected def Option.tryCatchStr (x : Option α) (handle : String → Option α) : Option α :=
  match x with
  | some _ => x
  | none => handle ""

instance : MonadExceptOf String Option where
  throw    := fun _ => Option.none
  tryCatch := Option.tryCatchStr

def isSAT? : Assignment -> Formula -> Option Bool := isSAT

def wmc_incomplete [Monad m] [MonadExceptOf String m] (map: WeightMap) : Formula -> m Float
  | .id i =>
    match map.find? i with
    | none => throw "error! no weight found in wmc map for {i}"
    | some _ => return 0
  | .bool b => return (if b then 1 else 0)
  | .disj l r => do
    let l <- wmc_incomplete map l
    let r <- wmc_incomplete map r
    return l+r
  | .conj l r => do
    let l <- wmc_incomplete map l
    let r <- wmc_incomplete map r
    return l+r
  | .neg b => wmc_incomplete map b

def isConsecutive [Monad m] [MonadExceptOf String m] (nats: List Nat) : m Bool :=
  match nats.maximum? with
  | none => throw "error: empty weight map in validVars"
  | some mx => pure $ nats.length - 1 == mx

#eval (isConsecutive [] : Except String Bool)
#eval (isConsecutive [2,1,3,0] : Except String Bool)

def validVars [Monad m] [MonadExceptOf String m] (weights: WeightMap) : m Bool :=
  map2vars weights |> HashSet.toList |> isConsecutive

def assertConsecutiveVars [Monad m] [MonadExceptOf String m] (weights: WeightMap) : m Unit := do
  let num_vars := weights.size - 1
  let isValid <- validVars weights
  if not isValid
  then
    let keys : List Nat := weights.toList.map Prod.fst
    throw s!"weight keys are not consecutive nats.\nExpected: 0-{num_vars}\nGot:{keys}"
  else
    pure ()

def wmc [Monad m] [MonadExceptOf String m] (weights: WeightMap) (f: Formula) : m Float := do
  assertConsecutiveVars weights
  pure $ Id.run do
    let num_vars := weights.size - 1
    let init : Float := 0
    Stream.fold (all_assignments num_vars) (fun tot assgn =>
      match isSAT? assgn f with
      | none => tot
      | some b => if not b then pure tot else Id.run do
        let cur <- tot
        (Function.comp (. + cur) Prod.snd) <$> (Array.foldl (fun (ix, v) p =>
          let (l, h) := weights.find! ix
          (ix+1, if p then l else h)
        ) ((0 : Nat), (1 : Float)) assgn)
    ) init

def bernoulli [Monad m] [MonadStateOf Env m] (theta : Prob) : m Bool := do
  let env <- get
  let mx := stdRange.snd
  let (n, rgen) := randNat env.rgen 0 mx
  set { env with rgen := rgen }
  return n.toFloat / mx.toFloat > theta.prob

def evalANF [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] : ANF -> WeightMap -> SubstMap -> m Compiled
  | ANF.var v, m, p => do
    let v <- getFresh v
    return { (Monoid.unit : Compiled) with global :=   Formula.id v, weightMap := m, substitutions := p }
  | ANF.val (Val.bool b), m, p   => return { (Monoid.unit : Compiled) with global := Formula.bool b, weightMap := m, substitutions := p }
  | ANF.val (Val.prod l r), m, p => do
    let l' <- evalANF (ANF.val l) m p
    let r' <- evalANF (ANF.val r) m p
    return Monoid.op l' r'

open Formula in
def evalExpr [Monad m] [MonadStateOf Env m] [MonadExceptOf String m] : Expr -> WeightMap -> SubstMap -> m Compiled
  | Expr.anf a, m, p => evalANF a m p
  | Expr.fst a, m, p => evalANF a m p -- FIXME not doing any type-checking, so this is too weak
  | Expr.snd a, m, p => evalANF a m p -- FIXME not doing any type-checking, so this is too weak
  | Expr.prod l r, m, p => do
    let l <- evalANF l m p
    let r <- evalANF r m p
    return Monoid.op l r
  | Expr.letIn x e rest, m, p => do
    let x <- getId x -- FIXME this is a bug, the map is always empty
    let comp_1 <- evalExpr e m p
    let comp_2 <- evalExpr rest comp_1.weightMap (comp_1.substitutions.insert x comp_1.global)
    return { global        := comp_1.global /\ (comp_2.global.subst x comp_1.global)
           , accept        := comp_1.accept /\ (comp_2.accept.subst x comp_1.accept)
           , weightMap     := comp_1.unionMaps comp_2
           , substitutions := comp_1.unionSubs comp_2
           , probability   := comp_1.probability.mul comp_2.probability
           , importanceWeight := comp_1.importanceWeight * comp_2.importanceWeight
           }
  | Expr.ite pred t f, m, p => do
    let a <- evalANF pred m p
    let comp_t <- evalExpr t m p
    let comp_f <- evalExpr f m p
    return { global := (a.global /\ comp_t.global) \/ (neg a.global /\ comp_f.global)
           , accept := (a.accept /\ comp_t.accept) \/ (neg a.accept /\ comp_f.accept)
           , weightMap     := comp_t.unionMaps comp_f
           , substitutions := comp_t.unionSubs comp_f
           , probability   := comp_t.probability.add comp_f.probability
           , importanceWeight := comp_t.convexCombIW comp_f
           }
  | Expr.flip param, m, p => do
    let f <- fresh
    let flipMap := HashMap.empty.insert f (1-param, param)
    return { global := id f
           , accept := bool true
           , weightMap     := flipMap.mergeWith leftEntry m
           , substitutions := p
           , probability   := Prob.mk 1
           , importanceWeight := 1
           }
  | Expr.observe a, m, p => do
    let acomp <- evalANF a m p
    let w <- wmc m (acomp.global.apply p)
    return { global := bool true
           , accept := acomp.global
           , weightMap     := m
           , substitutions := p
           , probability   := Prob.mk 1
           , importanceWeight := w
           }
  | Expr.sample e, m, p => do
    let ecomp <- evalExpr e m p
    if ecomp.accept != bool true then throw "sample statement includes observe statement" else
    let theta_q <- wmc ecomp.weightMap ecomp.global
    let v <- bernoulli (Prob.mk theta_q)
    let q := Prob.mk (if v then theta_q else 1-theta_q)
    return { global := bool true
           , accept := ecomp.global
           , weightMap     := m
           , substitutions := p
           , probability   := q
           , importanceWeight := 1
           }

