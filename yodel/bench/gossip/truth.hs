module Main where

import System.Environment (getArgs)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Ratio

type Node = Int
type Steps = Int

main :: IO ()
main = do
   arg :: [Int] <- fmap read <$> getArgs
   case arg of
     [n, s] -> do
       let rtl = expect n s (paths n s)
       let render = show (numerator rtl) <> "/" <> show (denominator rtl)
       let float = show $ (fromIntegral (numerator rtl) :: Float) / fromIntegral (denominator rtl)
       putStrLn $ render <> " = " <> float
     _ -> putStrLn "need two numbers: # nodes, # steps!"

expect :: Node -> Steps -> [IntSet] -> Rational
expect mx s seen
  = fromIntegral (sum (fmap IntSet.size seen))
  % (fromIntegral mx - 1) ^ s

paths :: Node -> Steps -> [IntSet]
paths mx s = uncurry IntSet.insert <$> paths' [(0, IntSet.empty)] mx s

paths' :: [(Node, IntSet)] -> Node -> Steps -> [(Node, IntSet)]
paths' ps mx   0 = ps
paths' ps mx gas = paths' next mx (gas - 1)
  where
    next :: [(Node, IntSet)]
    next = concatMap go ps

    go :: (Node, IntSet) -> [(Node, IntSet)]
    go (fr, seen) = fmap (, IntSet.insert fr seen) (neighbors fr)

    neighbors :: Node -> [Node]
    neighbors n = filter (/= n) [0..mx-1]
