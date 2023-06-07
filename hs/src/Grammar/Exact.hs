{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Grammar.Exact where

import Control.Monad.Free
import Control.Monad.Meas
import Control.Monad.State.Strict
import Control.Monad.Weighted
import Data.Maybe ()
import Data.Ratio ()
import GHC.Natural
import Prelude.Unicode

data Val where
  T :: Val
  F :: Val
  Bern :: Natural -> Rational -> Val
  And :: Val -> Val -> Val
  Or :: Val -> Val -> Val
  Not :: Val -> Val
  ConditionedOn :: Val -> Val -> Val
  deriving (Eq)

_dirac :: Val -> Val -> Rational
_dirac base v = if v == base then 1 else 0

_bern :: Rational -> Val -> Rational
_bern θ = \case
  T -> θ
  F -> 1 - θ
  _ -> 0

asDist :: Val -> (Val -> Rational)
asDist = \case
  T -> _dirac T
  F -> _dirac F
  Bern _ θ -> _bern θ
  And l r -> (\v -> asDist l v * asDist r v)
  Not p -> (\v -> 1 - asDist p v)
  -- FIXME: the following are wrong, but I don't really have a weighted BDD
  -- library to work with.
  Or l r -> (\v -> asDist l v * asDist r v)
  ConditionedOn dist g -> \v ->
    let cond = asDist g v
     in let p = asDist dist v
         in p * cond

-- | Let and return (or "query") are covered in the monadic bind so we omit them here
data ExactF next
  = Flip Rational (Val -> next)
  | Observe Val (() -> next)
  | ITE Val next next (Val -> next)
  deriving (Functor)

type Exact = Free ExactF

bern :: Rational -> Exact Val
bern θ = liftF $ Flip θ id

observe :: Val -> Exact ()
observe v = liftF $ Observe v id

ite :: Val -> Exact Val -> Exact Val -> Exact Val
ite v left right = do
  l <- left
  r <- right
  liftF $ ITE v l r id

evaluate :: Val -> Maybe Bool
evaluate = \case
  T -> Just True
  F -> Just False
  And l r -> (&&) <$> evaluate l <*> evaluate r
  Or l r -> (||) <$> evaluate l <*> evaluate r
  Not p -> not <$> evaluate p
  _ -> Nothing

twoCoinFlips :: Exact Val
twoCoinFlips = do
  x <- bern (1 / 3)
  y <- bern (1 / 4)
  return (Or x y)

twoObservedCoinFlips :: Exact Val
twoObservedCoinFlips = do
  x <- bern (1 / 3)
  y <- bern (1 / 4)
  observe (Or x y)
  return x

choiceCoinFlips :: Exact Val
choiceCoinFlips = do
  p <- bern (1 / 3)
  o <- ite p (bern (1 / 4)) (bern (3 / 4))
  observe o
  return p

-- natural transformation
type f ~> g = forall x. f x -> g x

fresh :: MonadState Natural m => m Natural
fresh = modify (+ 1) >> get

newtype Dist a = Dist {unDist :: StateT Natural Meas a}
  deriving (Functor, Applicative, Monad, MonadState Natural)

-- FIXME: Dist is not general enough to turn this into a natural transformation.
-- Probably need to bump this up to Meas.
evalExact :: Exact Val -> Weighted Dist Val
evalExact = iterM alg
  where
    alg :: ExactF (Weighted Dist Val) -> Weighted Dist Val
    alg (Flip θ cb) = do
      lbl <- lift fresh
      cb (Bern lbl θ)
    alg (Observe c cb) = do
      v <- cb ()
      pure $ v `ConditionedOn` c
    alg (ITE p ml mr cb) = do
      l <- ml
      r <- mr
      cb $ (p `And` l) `Or` (Not p `And` r)

unweight :: Weighted Dist Val -> Dist Val
unweight = flip evalStateT one . unWeight

measure :: Dist Val -> Meas Val
measure x = evalStateT (unDist x) 0

query :: Meas Val -> Val -> Rational
query m q = integrate (`asDist` q) m

run :: Exact Val -> (Val -> Rational)
run = query ∘ measure ∘ unweight ∘ evalExact

main :: IO ()
main = do
  let queryFlips = run twoCoinFlips
  print $ queryFlips T
  print $ queryFlips F
