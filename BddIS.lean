import Std.Data.HashMap
open Std (HashMap)
open Option
open Prod

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
abbrev V := Val

inductive ANF where
  | var (s:String) : ANF
  | val (v:Val) : ANF
deriving Repr, BEq
abbrev A := ANF

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
abbrev E := Expr

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
abbrev F := Formula

def Formula.subst (f: Formula) (x: Nat) (v: Formula) :=
  match f with
  | id _ => v
  | bool b => bool b
  | disj l r => disj (l.subst x v) (r.subst x v)
  | conj l r => conj (l.subst x v) (r.subst x v)
  | neg b => neg (b.subst x v)

abbrev WeightMap := HashMap (Prod Nat Bool) Float
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

structure Density where
  density : Float
deriving Repr

def leftEntry {a b : Type} (_ : a) (left : b) (_ : b) : b := left

structure Compiled where
  global : Formula
  accept : Formula
  weightMap : WeightMap
  substitutions : SubstMap
  probability : Prob
  importanceWeight : Float

def Compiled.unionMaps (l: Compiled) (r:Compiled) := l.weightMap.mergeWith leftEntry r.weightMap
def Compiled.unionSubs (l: Compiled) (r:Compiled) := l.substitutions.mergeWith leftEntry r.substitutions
def Compiled.convexCombIW (l: Compiled) (r:Compiled) := l.probability.prob * l.importanceWeight + r.probability.prob * r.importanceWeight

class Monoid (α : Type u) where
  unit : α
  op   : α → α → α

instance : Monoid Compiled where
  unit := { global := Formula.bool true, accept := Formula.bool true, weightMap := HashMap.empty, substitutions := HashMap.empty, probability := Prob.mk 1, importanceWeight := 1 }
  op l r := { global := Formula.conj l.global r.global, accept := Formula.conj l.accept r.accept, weightMap := l.unionMaps r, substitutions := l.unionSubs r, probability := Prob.mul l.probability r.probability, importanceWeight := l.importanceWeight * r.importanceWeight }


def wmc_incomplete [Monad m] [MonadExceptOf String m] (map: WeightMap) : Formula -> m Float
  | .id i =>
    match map.find? (i, true) with
    | none => throw "error! no weight found in wmc map for {i}"
    | some x => return 0
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

def wmc [Monad m] [MonadExceptOf String m] : WeightMap -> Formula -> m Float := wmc_incomplete

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
    let flipMap := (HashMap.empty.insert (f, true) param).insert (f, false) (1-param)
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

