{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UnicodeSyntax #-}

module Grammar.Sample where

import Control.Monad.Free
import Control.Monad.State.Strict
import Data.Maybe
import Data.Ratio
import GHC.Natural
import Prelude.Unicode
import System.Random.MWC.Probability

data Val = T | F
  deriving (Eq, Show)

fromBool :: Bool -> Val
fromBool True = T
fromBool False = F

data SampleF next
  = Flip Rational (Val -> next)
  deriving (Functor)

type Sample = Free SampleF

bern :: Rational -> Sample Val
bern θ = liftF $ Flip θ id

twoCoinFlips :: Sample [Val]
twoCoinFlips = do
  x <- bern (1 / 3)
  y <- bern (1 / 4)
  return [x, y]

evalSample :: Sample x -> Prob IO x
evalSample = iterM alg
  where
    alg :: SampleF (Prob IO x) -> Prob IO x
    alg (Flip θ cb) =
      bernoulli (fromRational θ)
        >>= pure ∘ fromBool
        >>= cb

run :: Sample x -> IO x
run s = createSystemRandom >>= \g -> sample (evalSample s) g

main :: IO ()
main = do
  run twoCoinFlips >>= putStrLn ∘ show
