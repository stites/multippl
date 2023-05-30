{-# Language InstanceSigs #-}
{-# Language NoImplicitPrelude #-}
{-# Language ScopedTypeVariables #-}
module MyLib where

import Prelude hiding (and, or, not)
import Data.Functor.Identity
import Control.Monad.Free
import Control.Monad.State.Strict
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

data FormulaF next
  = Set String Bool next
  | Var String (Bool -> next)
  | Val Bool (Bool -> next)
  | And (Bool -> Bool -> next)
  | Or  (Bool -> Bool -> next)
  | Not next
  deriving Functor

type Formula = Free FormulaF

set :: String -> Bool -> Formula ()
set x v = Free (Set x v (pure ()))

var :: String -> Formula Bool
var x = liftF $ Var x id

val :: Bool -> Formula Bool
val x = liftF $ Val x id

and, or :: Formula Bool
and = Free $ And (\l r -> pure (l && r))
or = Free $ Or (\l r -> pure (l || r))

-- not :: Formula x -> Formula x
-- not p = Free $ Not p

-- example :: Formula Bool
-- example = do
--   set "x" False
--   set "y" True
--   and (or (val True) (var "x")) (var "y")

asState :: FormulaF ~> State (HashMap String Bool, Bool)
asState (Set x v cb) = do
  (m, s) <- get
  put (HM.insert x v m, s)
  pure cb
asState (Var x cb) =
  gets (\m -> fromJust (HM.lookup x (fst m))) >>= pure . cb
asState (Val x cb) = pure $ cb x
asState (And cb) = do
  l' <- pure l
  r' <- pure r
  (m, s) <- get
  put (m, s && l' && r')



-- foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a

-- eval :: Formula a -> State (HashMap String Bool) a
-- eval = foldFree go
--   where
--     go :: forall x . Formula x -> State (HashMap String Bool) x
--     go
-- eval :: Formula Bool -> Bool
-- eval formula = runIdentity $ evalState mempty go where
--   go :: StateC (HashMap String Bool) Identity Bool
--   go = undefined


data DiceF next
  = Flip Double
  | ObserveD (DiceF next)
  | LetInD String (DiceF next) next
  -- | QueryF Formula
  deriving Functor

type Dice = Free DiceF

data YodelF next
  = Sample next
  | ObserveY (YodelF next)
  | LetInY String (YodelF next) next
  deriving Functor

type Yo = Free YodelF

-- Our natural transformation for interpreting
type f ~> g = forall x . f x -> g x
