#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [megaparsec parser-combinators unordered-containers])"
#! nix-shell -i "ghcid -c 'ghci' -T main"
-- #! nix-shell -i "runhaskell"
-- #! /usr/bin/env -S"ANSWER=42" nix-shell
-- #! nix-shell -p ghcid
-- #! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [vector containers megaparsec])"
-- #! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

-- import Text.Megaparsec -- hiding (many, manyTill_)
-- import Text.Megaparsec.Char as P
import Data.Void
import Debug.Trace
import qualified Data.HashSet as HS
import Control.Monad
import Control.Applicative.Combinators
line0 = "def stuff ():"
itesmall = "if ((N34StarFcst == 0)) then (discrete(0.000000,0.000000,1.000000)) else (((discrete(0.000000,0.000000,1.000000))"
itelarge = "if ((MountainFcst == 0)) then (if ((N34StarFcst == 0)) then (discrete(1.000000,0.000000,0.000000)) else (((discrete(0.000000,1.000000,0.000000))) if ((N34StarFcst == 1)) else ((discrete(0.000000,0.000000,1.000000))))) else (if ((MountainFcst == 1)) then (if ((N34StarFcst == 0)) then (discrete(0.000000,1.000000,0.000000)) else (((discrete(0.000000,1.000000,0.000000))) if ((N34StarFcst == 1)) else ((discrete(0.000000,0.000000,1.000000))))) else (if ((N34StarFcst == 0)) then (discrete(0.000000,0.000000,1.000000)) else (((discrete(0.000000,0.000000,1.000000))) if ((N34StarFcst == 1)) else ((discrete(0.000000,0.000000,1.000000))))))"
line1 = "  R5Fcst = " ++ itelarge ++ " # comment"
line2 = "    foo = " ++ itesmall
line3 = "  return things"
fulltest = unlines
  [ line0
  , line1
  , line2
  , line3
  ]

data ITE = ITE { p :: String, t :: Either String ITE, f :: Either String ITE }
  deriving (Show,Eq)
type VarLine = (String, ITE, String)
type Line = String
type L = [Either Line VarLine]

spacewords :: String -> [String]
spacewords = go [] ""
  where
    go rs (' ':cur) (' ':rst) = go                       rs  (' ':' ':cur) rst
    go rs (  a:cur) (' ':rst) = go ((reverse (   a:cur)):rs)         [' '] rst
    go rs (' ':cur)   (a:rst) = go ((        ( ' ':cur)):rs)         [ a ] rst
    go rs (  b:cur)   (a:rst) = go                       rs  (  a:  b:cur) rst
    go rs        []   (a:rst) = go                       rs          [ a ] rst
    go rs      cur         [] = reverse (reverse cur:rs)

isIf = flip HS.member (HS.fromList ["if", "(if", "((if", "(((if", "(((((if", "((((((if"
                                   ," if", " (if", " ((if", " (((if", " (((((if", " ((((((if"])
parseIte :: [String] -> Maybe (ITE, [String])
parseIte ("if":rst) = do
  (ps, "then":rst) <- Just $ break (== "then") rst
  let p = concat ps
  -- traceM $ p
  (ts, "else":rst) <- Just $ break (== "else") rst
  let tb = maybe (Left $ concat ts) (\(i, []) -> Right i) (parseIte ts)
  -- traceM $ show tb
  (fs, rst) <- Just $ break isIf rst
  let fb = maybe (Left $ concat fs) (\(i, []) -> Right i) (parseIte fs)
  -- traceM $ show fb
  pure (ITE p tb fb, rst)
parseIte _ = Nothing

parseIteLine :: [String] -> Maybe ([String], ITE, [String])
parseIteLine rst = case break (== "if") rst of
  (ps, []) -> Nothing
  (ps, "if":rst) -> do
    (i, rst) <- parseIte ("if":rst)
    pure (ps, i, rst)

parseLine :: [String] -> (Either [String] ([String], ITE, [String]))
parseLine line = maybe (Left line) (Right) (parseIteLine line)

parseFile :: String -> [Either [String] ([String], ITE, [String])]
parseFile s = map (parseLine . spacewords) (lines s)

trinaryITE :: ITE -> String
trinaryITE (ITE p t f) = unwords ["(", branch t, ")", "if", p, "else", "(", branch f, ")"]
  where branch (Left s) = s
        branch (Right s) = trinaryITE s

renderFile :: String -> String
renderFile s = unlines $ go <$> parseFile s
  where go (Left ss) = concat ss
        go (Right (l, i, r)) = concat l ++ trinaryITE i ++ concat r


parseTest :: Show a => ([String] -> Maybe a) -> String -> IO ()
parseTest p s = do
  print $ p (spacewords s)

parseT :: ([String] -> Maybe ([String], ITE, [String])) -> String -> IO ()
parseT p s = do
  case p (spacewords s) of
    Just (l, i, r) -> do
      print l
      print i
      print r


main :: IO ()
main = do
  -- parseTest parseIte itesmall
  -- parseTest parseIte itelarge
  parseT parseIteLine line1
  -- parseTest parseIteLine line1
  -- parseTest (Just . parseLine) line0
  -- putStrLn $ renderFile fulltest
