{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

module MLS where

import Control.Monad.Free
import Control.Monad.Meas
import Control.Monad.State.Strict
import Control.Monad.Weighted
import Data.Maybe ()
import Data.Ratio ()
import GHC.Natural
import GHC.Utils.Monad ()
import Prelude.Unicode ((∘))
import System.Random.MWC
import System.Random.MWC.Distributions

newtype G = G {unG :: GenIO}

newtype Seeded m a = Seeded {unSeed :: StateT G m a}
  deriving (Functor, Applicative, Monad, MonadState G)

gen :: MonadState G m => m GenIO
gen = unG <$> get

instance MonadTrans Seeded where
  lift :: Monad m => m a -> Seeded m a
  lift m = Seeded $ StateT (\s -> (,s) <$> m)

instance MonadIO io => MonadIO (Seeded io) where
  liftIO :: IO a -> Seeded io a
  liftIO act = Seeded $ StateT (\s -> (,s) <$> liftIO act)

data SVal = SBool Bool
  deriving (Eq, Show)

fromBool :: Bool -> SVal
fromBool = SBool

data SampleF next
  = SFlip Rational (SVal -> next)
  | SExact (Exact EVal) (SVal -> next)
  deriving (Functor)

type Sample = Free SampleF

sbern :: Rational -> Sample SVal
sbern θ = liftF $ SFlip θ id

stwoCoinFlips :: Sample [SVal]
stwoCoinFlips = do
  x <- sbern (1 / 3)
  y <- sbern (1 / 4)
  return [x, y]

weightBy :: MonadState PQ m => Rational -> Bool -> m ()
weightBy θ b = modify (* asPQ (if b then θ else 1 - θ))

-- FIXME: missing accepting criteria
evalSample :: Sample ~> Target
evalSample = iterM alg
  where
    alg :: SampleF (Target x) -> Target x
    alg (SFlip θ cb) = do
      g <- lift . lift $ gen
      b <- liftIO (bernoulli (fromRational θ) g)
      weightBy θ b
      cb (fromBool b)
    alg (SExact e cb) = do
      ev <- evalExact e
      let θ = asDist ev T
      g <- lift . lift $ gen
      b <- liftIO (bernoulli (fromRational θ) g)
      weightBy θ b
      cb $ fromBool b

type Target = Weighted (DistT (Seeded IO))

evalTarget :: Target ~> IO
evalTarget t = do
  g <- createSystemRandom
  (PQ (p, q), dx :: DistT (Seeded IO) x) <- runWeighted t
  pure undefined

-- evalDistT :: (a -> m Rational) -> DistT m a -> m Rational
-- (DistT (Seeded IO))
-- sample (evalSample s) g

srun :: Sample x -> IO x
srun s = do
  g <- createSystemRandom
  sample (evalSample s) g

data EVal where
  T :: EVal
  F :: EVal
  Bern :: Natural -> Rational -> EVal
  And :: EVal -> EVal -> EVal
  Or :: EVal -> EVal -> EVal
  Not :: EVal -> EVal
  ConditionedOn :: EVal -> EVal -> EVal
  deriving (Eq)

_dirac :: Eq a => a -> a -> Rational
_dirac base v = if v == base then 1 else 0

_bern :: Rational -> EVal -> Rational
_bern θ = \case
  T -> θ
  F -> 1 - θ
  _ -> 0

asDist :: EVal -> (EVal -> Rational)
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
  = EFlip Rational (EVal -> next)
  | Observe EVal (() -> next)
  | ITE EVal next next (EVal -> next)
  | ESamp (Sample SVal) (EVal -> next)
  deriving (Functor)

type Exact = Free ExactF

bern :: Rational -> Exact EVal
bern θ = liftF $ EFlip θ id

observe :: EVal -> Exact ()
observe v = liftF $ Observe v id

ite :: EVal -> Exact EVal -> Exact EVal -> Exact EVal
ite v left right = do
  l <- left
  r <- right
  liftF $ ITE v l r id

evaluate :: EVal -> Maybe Bool
evaluate = \case
  T -> Just True
  F -> Just False
  And l r -> (&&) <$> evaluate l <*> evaluate r
  Or l r -> (||) <$> evaluate l <*> evaluate r
  Not p -> not <$> evaluate p
  _ -> Nothing

twoCoinFlips :: Exact EVal
twoCoinFlips = do
  x <- bern (1 / 3)
  y <- bern (1 / 4)
  return (Or x y)

twoObservedCoinFlips :: Exact EVal
twoObservedCoinFlips = do
  x <- bern (1 / 3)
  y <- bern (1 / 4)
  observe (Or x y)
  return x

choiceCoinFlips :: Exact EVal
choiceCoinFlips = do
  p <- bern (1 / 3)
  o <- ite p (bern (1 / 4)) (bern (3 / 4))
  observe o
  return p

-- natural transformation
type f ~> g = forall x. f x -> g x

fresh :: MonadState Natural m => m Natural
fresh = modify (+ 1) >> get

newtype DistT m a = DistT {unDistT :: StateT Natural (MeasT m) a}
  deriving (Functor, Applicative, Monad, MonadState Natural)

instance MonadTrans DistT where
  lift :: Monad m => m a -> DistT m a
  lift m = DistT $ StateT (\s -> (,s) <$> lift m)

instance MonadIO io => MonadIO (DistT io) where
  liftIO :: IO a -> DistT io a
  liftIO act = DistT $ StateT (\s -> (,s) <$> liftIO act)

evalDistT :: (a -> m Rational) -> DistT m a -> m Rational
evalDistT δ = categoricalMeasT δ . flip evalStateT 0 ∘ unDistT

-- FIXME: Dist is not general enough to turn this into a natural transformation.
-- Probably need to bump this up to Meas.
evalExact :: Exact EVal -> Target EVal
evalExact = iterM alg
  where
    alg :: ExactF (Target EVal) -> Target EVal
    alg _ = undefined

-- alg (EFlip θ cb) = do
--   lbl <- fresh
--   cb (Bern lbl θ)
-- alg (Observe c cb) = do
--   v <- cb ()
--   pure $ v `ConditionedOn` c
-- alg (ITE p ml mr cb) = do
--   l <- ml
--   r <- mr
--   cb $ (p `And` l) `Or` (Not p `And` r)
-- alg (ESamp e cb) =
--   evalSample e >>= \(SBool b) -> cb (if b then T else F)

-- unweight :: Weighted (DistT m) EVal -> DistT m EVal
-- unweight = flip evalStateT one . unWeight

-- measure :: DistT m EVal -> MeasT m EVal
-- measure x = evalStateT (unDistT x) 0

-- query :: Monad m => MeasT m EVal -> EVal -> m Rational
-- query m q = integrateT (\v -> pure $ (asDist v) q) m

-- run :: Monad m => Exact EVal -> (EVal -> m Rational)
-- run = query ∘ measure ∘ unweight ∘ eval
--   where
--     eval :: Exact EVal -> Weighted (DistT m) EVal
--     eval = evalExact

-- -- main :: IO ()
-- -- main = do
-- --   run twoCoinFlips T >>= print
-- --   run twoCoinFlips F >>= print
