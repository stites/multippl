module Main where

import Grammar.Exact qualified
import Grammar.Sample qualified

main :: IO ()
main = do
  putStrLn "exact tests:"
  Grammar.Exact.main
  putStrLn ""
  putStrLn "sample tests:"
  Grammar.Sample.main
