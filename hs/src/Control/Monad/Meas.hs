module Control.Monad.Meas where

import Control.Monad.Identity
import Control.Monad.Trans

type Meas = MeasT Identity

newtype MeasT m a = MeasT {unMeasT :: (a -> m Rational) -> m Rational}

instance Functor (MeasT m) where
  fmap a2b ν = MeasT $ \bweight -> integrateT (bweight . a2b) ν

instance Applicative (MeasT m) where
  pure a = MeasT (\weight -> weight a)
  fa2b <*> fa = MeasT $ \bweight -> do
    integrateT (\a2b -> integrateT (bweight . a2b) fa) fa2b

instance Monad (MeasT m) where
  ma >>= a2mb = MeasT $ \bweight ->
    integrateT (integrateT bweight . a2mb) ma

instance MonadTrans MeasT where
  lift :: Monad m => m a -> MeasT m a
  lift ma = MeasT $ \weight -> ma >>= weight

instance MonadIO io => MonadIO (MeasT io) where
  liftIO :: IO a -> MeasT io a
  liftIO act = MeasT $ \weight -> liftIO act >>= weight

integrateT :: (a -> m Rational) -> MeasT m a -> m Rational
integrateT δa (MeasT integral) = integral δa

integrate :: (a -> Rational) -> Meas a -> Rational
integrate δa ν = runIdentity $ integrateT (pure . δa) ν

categoricalMeasT :: Monad m => (a -> m Rational) -> MeasT m a -> m Rational
categoricalMeasT δa m = unMeasT m δa
