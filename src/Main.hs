module Main where

import Data.List
import Data.Maybe
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Exception
import Control.Monad
import Data.Function

-- FIXME: just guessing freqs
ltrByFreq = "EAIOU" ++ "RSTLN" ++ "YBCDFGHJKMPWQVXZ"
-- so much for freq..
ltrs = S.fromList ltrByFreq

-- my default dictionary is OWL + ['A','I','O']
lexFN = "/usr/share/dict/scrabble1"

data CharPoss = Ch Char | Var Char deriving Eq

pam :: a -> [a -> b] -> [b]
pam x fs = [f x | f <- fs]

findWds :: T.Trie Char () -> S.Set Char -> M.Map Char Char -> String ->
  [(M.Map Char Char, S.Set Char)]
findWds lex poss dec s@(x:xs) = case M.lookup x dec of
  Just c -> case T.descend c lex of
    Just lex' -> findWds lex' poss dec xs
    Nothing -> []
  Nothing -> concatMap
    (\ l -> findWds lex (S.delete l poss) (M.insert x l dec) s) $ S.elems poss
findWds lex poss dec [] = case T.lookup [] lex of
  Just _ -> [(dec, poss)]
  Nothing -> []

-- future overhaul idea for greater efficiency:
-- at each step proceed with the first letter of a word that introduces the
-- fewest new possibilities
--findWdss :: S.Set Char -> M.Map Char Char -> [(T.Trie Char (), String)] -> S.Set Char -> M.Map Char Char -> [String] ->
--  [M.Map Char Char]

findWdss :: T.Trie Char () -> S.Set Char -> M.Map Char Char -> [String] ->
  [(M.Map Char Char, S.Set Char)]
findWdss lex poss dec (wd:wds) = concat
  [findWdss lex poss' dec' wds | (dec', poss') <- findWds lex poss dec wd]
findWdss _ poss dec [] = [(dec, poss)]

doMap :: (Ord a) => M.Map a a -> [a] -> [a]
doMap m = map (\ k -> case  M.lookup k m of
  Nothing -> k
  Just v -> v
  )

getLex :: FilePath -> IO (T.Trie Char ())
getLex fN = do
  hPutStrLn stderr "loading lexicon"
  h <- openFile fN ReadMode
  c <- B.hGetContents h
  let ls = B.lines c
      lex = T.fromList . map (flip (,) ()) . map B.unpack $ ls
  -- does this do anything?  like speed up by loading into memory
  evaluate lex
  hPutStrLn stderr "loaded lexicon"
  return lex

decode :: T.Trie Char () -> String -> IO ()
decode lex line = do
  let
    -- TODO: we can probably get big gains from word ordering
    --wds = reverse . sortBy (compare `on` length) $ words s
    --wds = sortBy (compare `on` length) $ words s
    wds = words line
    poss = map fst $ findWdss lex ltrs M.empty wds
  putStr . unlines $ pam line (map doMap poss)
  putStrLn "."

main :: IO ()
main = do
  lex <- getLex lexFN
  inp <- getContents
  mapM_ (decode lex) $ lines inp
