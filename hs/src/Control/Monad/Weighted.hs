module Control.Monad.Weighted where

import Control.Monad.State.Strict
import GHC.Utils.Monad ()

newtype PQ = PQ (Rational, Rational)

instance Num PQ where
  (PQ (a0, b0)) + (PQ (a1, b1)) = PQ (a0 + a1, b0 + b1)
  (PQ (a0, b0)) - (PQ (a1, b1)) = PQ (a0 - a1, b0 - b1)
  (PQ (a0, b0)) * (PQ (a1, b1)) = PQ (a0 * a1, b0 * b1)
  negate (PQ (a, b)) = PQ (negate a, negate b)
  abs (PQ (a, b)) = PQ (abs a, abs b)
  signum (PQ (a, b)) = PQ (signum a, signum b)
  fromInteger i = PQ (fromInteger i, fromInteger i)

zero :: PQ
zero = PQ (0, 0)

one :: PQ
one = PQ (1, 1)

asPQ :: Rational -> PQ
asPQ r = PQ (r, r)

runWeighted :: Weighted m a -> m (a, PQ)
runWeighted = flip runStateT one . unWeight

newtype Weighted m a = Weighted {unWeight :: StateT PQ m a}
  deriving (Functor, Applicative, Monad, MonadState PQ)

instance MonadTrans Weighted where
  lift :: Monad m => m a -> Weighted m a
  lift m = Weighted $ StateT (\s -> (,s) <$> m)

instance MonadIO io => MonadIO (Weighted io) where
  liftIO :: IO a -> Weighted io a
  liftIO act = Weighted $ StateT (\s -> (,s) <$> liftIO act)
