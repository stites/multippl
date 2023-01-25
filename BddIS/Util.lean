import Std
open Std

def abs (n : Int) : Nat := Int.toNat <| if n < 0 then -n else n
def sgn (n : Int) : Int := if n > 0 then 1 else if n == 0 then 0 else -1
def Array.sum [Add α] [OfNat α 0] (a : Array α) : α := a.foldl (· + ·) 0
def Array.prod [Mul α] [OfNat α 1] (a : Array α) : α := a.foldl (· * ·) 1
def Array.min (a : Array Nat) : Nat := a.foldl _root_.min a[0]
def Array.max (a : Array Nat) : Nat := a.foldl _root_.max 0

partial def List.perms [DecidableEq α] : List α → List (List α)
  | [] => [[]]
  | as => as.bind (fun a => perms (as.filter (· ≠ a)) |>.bind (fun perm => [a::perm]))

partial def List.windowed : List α → List (α × α)
  | a::b::cs => (a, b) :: windowed (b::cs)
  | _ => []

def Array.windowed (n : Nat) (as : Array α) : Array (Subarray α) := Id.run do
  let mut r := #[]
  for i in [0:as.size + 1 - n] do
    r := r.push as[i:i + n]
  return r

partial def List.pairs (as : List α) (bs : List β) : List (α × β) :=
  as.bind (fun a => bs.map ((a, ·)))

instance : Hashable Char where
  hash c := hash c.val

structure Wrap (α : Type) where wrap :: val : α
export Wrap (wrap)

instance [Add α] : Add (Array α) where
  add as bs := as.zipWith bs Add.add

structure Loop where

@[inline]
partial def Loop.forIn {β : Type u} {m : Type u → Type v} [Monad m] (loop : Loop) (init : β) (f : Unit → β → m (ForInStep β)) : m β :=
  let rec @[specialize] loop (b : β) : m β := do
    match ← f () b with
      | ForInStep.done b  => pure b
      | ForInStep.yield b => loop b
  loop init

instance : ForIn m Loop Unit where
  forIn := Loop.forIn

syntax "repeat " doSeq : doElem

macro_rules
  | `(doElem| repeat $seq) => `(doElem| for _ in Loop.mk do $seq)

syntax "while " termBeforeDo " do " doSeq : doElem

macro_rules
  | `(doElem| while $cond do $seq) =>
    `(doElem| repeat if $cond then $seq else break)

def sqrLen (p : Array Int) := p[0] * p[0] + p[1] * p[1] + p[2] * p[2]
def manhattan (p : Array Int) := abs p[0] + abs p[1] + abs p[2]

def groupBy [Hashable β] [DecidableEq β] (f : α → β) (as : Array α) : HashMap β (Array α) := Id.run do
  let mut map := Std.HashMap.empty
  for a in as do
    let b := f a
    map := map.insert b (map.findD b #[] |>.push a)
  map

instance [Sub α] : Sub (Array α) where
  sub as bs := as.zipWith bs Sub.sub

instance [Mul α] : HMul α (Array α) (Array α) where
  hMul a bs := bs.map (a * ·)

def dot [Add α] [Mul α] [OfNat α 0] (as bs : Array α) := as.zipWith bs Mul.mul |>.sum
def cross (as bs : Array Int) := #[as[1] * bs[2] - as[2] * bs[1], as[2] * bs[0] - as[0] * bs[2], as[0] * bs[1] - as[1] * bs[0]]
def cardinals := #[#[1, 0, 0], #[-1, 0, 0], #[0, 1, 0], #[0, -1, 0], #[0, 0, 1], #[0, 0, -1]]

def Array.foldMap (f : α → Array β) (as : Array α) : Array β :=
  as.map f |>.foldl (· ++ ·) #[]

def Std.Range.all (r : Std.Range) (p : Nat → Bool) : Bool := Id.run do
  for i in r do
    if !p i then return false
  return true

def Subarray.getOp [Inhabited α] (self : Subarray α) (idx : Nat) : α :=
  self.as[self.start + idx]

def btn (b : Bool) : Nat := if b then 1 else 0

#eval "Hello, world!"
