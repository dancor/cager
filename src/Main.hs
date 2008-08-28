module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Trie as T

data Opts = Opts {
  optDict :: String,
  optIgnoreChars  :: String,
  optIgnoreWdIs   :: [Int],
  optIgnoreWdLens :: [Int],
  optIgnoreWdsWithChars :: String
  }

defOpts = Opts {
  optDict = "/usr/share/dict/words",
  optIgnoreChars  = ".,;-:!?\"",
  optIgnoreWdIs   = [],
  optIgnoreWdLens = [],
  optIgnoreWdsWithChars = ""
  }

breakMb :: (a -> Bool) -> [a] -> Maybe ([a],[a])
breakMb f l = let (x,y) = break f l in
  if null y then Nothing else Just (x,tail y)

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks f l = if null b then [a] else (a:breaks f (drop 1 b))
  where (a,b) = break f l

readIntList :: String -> [Int]
readIntList s = concatMap readMbRange $ breaks (== ',') s where
  readMbRange wd = case breakMb (== '-') wd of
    Nothing -> [read wd]
    Just (lo,hi) -> [read lo..read hi]

optFs :: [OptDescr (Opts -> Opts)]
optFs = [
  Option "d" ["dictionary"]
    (ReqArg (\ dict opts -> opts {optDict = dict}) "FILE")
    "file to read words from (one per line)",
  Option "c" ["ignore-chars"]
    (ReqArg (\ cs opts -> opts {optIgnoreChars = cs}) "CHARS")
    "ignore characters (i.e. punctuation)\n(default: .,;-:!?\")",
  Option "n" ["ignore-wd-nums"]
    (ReqArg
      (\ ns opts -> opts {optIgnoreWdIs = map (subtract 1) $ readIntList ns})
      "NUMS")
    "ignore words numbers (e.g. 1,3-5\nconsiders only 2nd and 6th-rest)",
  Option "l" ["ignore-wd-lens"]
    (ReqArg (\ ls opts -> opts {optIgnoreWdLens = readIntList ls})
      "LENS")
    "ignore word lengths (e.g. 1,3-5\nconsiders only 2 and > 5 letter wds)",
  Option "x" ["ignore-wds-with-chars"]
    (ReqArg (\ cs opts -> opts {optIgnoreWdsWithChars = cs})
      "CHARS")
    "ignore words with these characters\n(e.g. apostrophe if your dict\ndoesn't have contractions)"
  ]

-- FIXME: just guessing freqs
ltrByFreq = "EAIOU" ++ "RSTLN" ++ "YBCDFGHJKMPWQVXZ"
-- so much for freq..
ltrs = S.fromList ltrByFreq

data CharPoss = Ch Char | Var Char deriving Eq

pam :: a -> [a -> b] -> [b]
pam x fs = [f x | f <- fs]

findWds :: T.Trie Char () -> S.Set Char -> M.Map Char Char -> String ->
  [(M.Map Char Char,S.Set Char)]
findWds lex poss dec s@(x:xs) = case M.lookup x dec of
  Just c -> case T.descend c lex of
    Just lex' -> findWds lex' poss dec xs
    Nothing -> []
  Nothing -> concatMap
    (\ l -> findWds lex (S.delete l poss) (M.insert x l dec) s) $ S.elems poss
findWds lex poss dec [] = case T.lookup [] lex of
  Just _ -> [(dec,poss)]
  Nothing -> []

-- future overhaul idea for greater efficiency:
-- at each step proceed with the first letter of a word that introduces the
-- fewest new possibilities
--findWdss :: S.Set Char -> M.Map Char Char -> [(T.Trie Char (),String)] -> S.Set Char -> M.Map Char Char -> [String] ->
--  [M.Map Char Char]

findWdss :: T.Trie Char () -> S.Set Char -> M.Map Char Char -> [String] ->
  [(M.Map Char Char,S.Set Char)]
findWdss lex poss dec (wd:wds) = concat
  [findWdss lex poss' dec' wds | (dec',poss') <- findWds lex poss dec wd]
findWdss _ poss dec [] = [(dec,poss)]

doMap :: (Ord a) => M.Map a a -> [a] -> [a]
doMap m = map (\ k -> case  M.lookup k m of
  Nothing -> k
  Just v -> v
  )

getLex :: FilePath -> IO (T.Trie Char ())
getLex fN = do
  hPutStrLn stderr $ "Loading dictionary: " ++ show fN
  h <- openFile fN ReadMode
  c <- B.hGetContents h
  let ls = B.lines c
      lex = T.fromList . map (flip (,) ()) . map (map toUpper . B.unpack) $ ls
  -- does this do anything?  like speed up by loading into memory
  -- well it allows us to give user accurate loading time anyway
  evaluate lex
  return lex

filterOnGen :: (b -> Bool) -> ([a] -> [(b, a)]) -> [a] -> [a]
filterOnGen test gen = map snd . filter (test . fst) . gen

filterOn :: (b -> Bool) -> (a -> b) -> [a] -> [a]
filterOn test f = filterOnGen test (map (\ x -> (f x, x)))

decode :: Opts -> T.Trie Char () -> String -> IO ()
decode opts lex line = do
  let
    -- TODO: we can probably get big gains from word ordering?
    --wds = reverse . sortBy (compare `on` length) $ words s
    --wds = sortBy (compare `on` length) $ words s
    wds =
      filterOnGen (`notElem` optIgnoreWdIs opts) (zip [0..]) .
      filterOn (`notElem` optIgnoreWdLens opts) length .
      filter (all (`notElem` optIgnoreWdsWithChars opts)) .
      nub .
      map (filter (`notElem` optIgnoreChars opts)) $ words line
    poss = map fst $ findWdss lex ltrs M.empty wds
  hPutStrLn stderr $ "Considering wordset: " ++ show wds
  putStr . unlines $ pam line (map doMap poss)
  putStrLn "."

main :: IO ()
main = do
  args <- getArgs
  opts <- let usage = "" in case getOpt Permute optFs args of
    (o,[],[]) -> return (foldl (flip id) defOpts o)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage optFs))
  lex <- getLex $ optDict opts
  inp <- getContents
  mapM_ (decode opts lex) $ lines inp
