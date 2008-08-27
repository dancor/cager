{-# OPTIONS_GHC -O2 #-}

module Data.Trie where

--import Data.Binary
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Prelude hiding (lookup)

data Trie k v = Trie (Maybe v) !(M.Map k (Trie k v))

lookup :: (Ord k, Monad m) => [k] -> Trie k v -> m v
lookup (x:xs) (Trie _ m)        = lookup xs =<< M.lookup x m
lookup []     (Trie (Just v) _) = return v
lookup _      _                 = fail "not in trie"

empty = Trie Nothing M.empty

singleton :: [k] -> v -> Trie k v
singleton xs v = foldr ((Trie Nothing .) . M.singleton) (Trie (Just v) M.empty) xs

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert []     v (Trie _ m)  = Trie (Just v) m
insert (x:xs) v (Trie mv m) = Trie mv $! M.alter f x m
  where
    f Nothing  = Just $! singleton xs v
    f (Just t) = Just $! insert xs v t

fromList xs = L.foldl' (\ t (k, v) -> insert k v t) empty xs

descend :: (Monad m, Ord k) => k -> Trie k t -> m (Trie k t)
descend k (Trie _ m) = M.lookup k m

{-
instance (Ord k, Binary k, Binary v) => Binary (Trie k v) where
  put (Trie vMby rem) = do
    put vMby
    put rem
  get = do
    vMby <- get
    rem <- get
    return (Trie vMby rem)
-}
