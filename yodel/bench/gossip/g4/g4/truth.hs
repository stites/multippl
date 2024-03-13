module Main where

import System.Environment (getArgs)
import Control.Monad (forM_)
import Data.List

type Node = Integer
type Steps = Integer

main :: IO ()
main = do
   arg :: [Integer] <- fmap read <$> getArgs
   case arg of
     [n, s] -> do
       (forM_ (let ps = paths n s in zip ps (fmap (pathProb n) ps)) print)
       (forM_ (let ps = paths n s in gby $ zip (fmap (pathProb' n) ps) ps) print)
       let ps = paths n s
       let g = gby $ zip (fmap (pathProb' n) ps) ps
       print $ (1 +) $ sum $ fmap fst $ g

       print . sum . fmap (pathProb n) $ paths n s
     _ -> putStrLn "need two numbers: # nodes, # steps!"

gby stuff = filter (\(p, ns) -> 0 /= head ns) $ sortBy (\(l, ls) (r, rs) -> compare (head ls) (head rs)) stuff

pathProb :: Node -> [Node] -> Double
pathProb mx [] = 1.0
pathProb mx (0 : cond) = 1.0 * pathProb mx cond
pathProb mx (n : cond) = (1.0 / (fromIntegral mx - 1)) * pathProb mx cond

pathProb' :: Node -> [Node] -> Double
pathProb' mx path = (1.0 / (fromIntegral mx - 1)) ^ (fromIntegral (length path) - 1)


paths :: Node -> Steps -> [[Node]]
paths n s = [0] : paths' [[0]] n s

paths' :: [[Node]] -> Node -> Steps -> [[Node]]
paths' ps mx   0 = ps
paths' ps mx gas = paths' next mx (gas - 1)
  where
    next :: [[Node]]
    next = concatMap (fmap (uncurry (:)) . go) ps

    go :: [Node] -> [(Node, [Node])]
    go path = fmap (, path) (neighbors (head path))

    neighbors :: Node -> [Node]
    neighbors n = filter (/= n) [0..mx-1]


-- ------------
-- -- faster now

-- prob :: Node -> Node -> Double
-- prob mx 0 = 1.0
-- prob mx n = 1.0 / (fromIntegral mx - 1)


-- pathsFaster :: Node -> Steps -> Double
-- pathsFaster mx n s = 1 + sum $ fmap (\(p, n) -> p * prob mx n) $ pathsFaster' [(1.0, 0)] n s

-- pathsFaster' :: [(Double, Node)] -> Node -> Steps -> [(Double, Node)]
-- pathsFaster' ps mx   0 = ps
-- pathsFaster' ps mx gas = pathsFaster' next mx (gas - 1)
--   where
--     next :: [[Node]]
--     next = concatMap (fmap (uncurry (:)) . go) ps

--     go :: [Node] -> [(Node, [Node])]
--     go path = fmap (, path) (neighbors (head path))

--     neighbors :: Node -> [Node]
--     neighbors n = filter (/= n) [0..mx-1]
