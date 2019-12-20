module Avl (Avl, show, fmap, foldr, all,  find, fork, isEmpty, empty, ht, insert, delete, join) where

import Prelude hiding (foldr,all)
import Data.Foldable
import Data.Functor


data Avl a = Null | Fork Int (Avl a) a (Avl a) 

instance (Show a) => Show (Avl a) where
  show Null = "[]"
  show (Fork h Null y Null) = "["  ++ show y ++ "]"
  show (Fork h xt y zt) = "[" ++ show xt ++ show y ++ show zt ++ "]"

empty = Null

-- instances of the Functor type class have map functions
-- fmap is like map for Avl trees.
instance Functor Avl where
  fmap f Null = Null
  fmap f (Fork h xt y zt) = Fork h (fmap f xt) (f y) (fmap f zt)  

-- instances of the Foldable type class have a fold function
-- here is a foldr for an Avl tree
instance Foldable Avl where
  foldr f id  Null = id
  foldr f id (Fork h xt y zt) = foldr f (f y (foldr f id zt)) xt

fork :: (Avl a) -> a -> (Avl a) -> (Avl a)
fork xt y zt = Fork h xt y zt
   where h = 1 + max (ht xt) (ht zt)

isEmpty Null = True
isEmpty _ = False

ht :: (Avl a) -> Int
ht Null = 0
ht (Fork h _ _ _) = h

insert :: (Ord a) => a -> Avl a -> Avl a
insert x Null = fork Null x Null
insert x (Fork h xt y zt) 
  | (x < y) = rebalance (insert x xt) y zt
  | (x == y) = Fork h xt x zt
  | (x > y) = rebalance xt y (insert x zt)

delete :: (Ord a) => a -> Avl a -> Avl a
delete x Null = Null
delete x (Fork h xt y zt) 
  | (x < y) = rebalance (delete x xt) y zt
  | (x == y) = join xt zt
  | (x > y) = rebalance xt y (delete x zt)

join :: Avl a -> Avl a -> Avl a
join xt yt = if isEmpty yt then xt else rebalance xt y zt
                 where (y,zt) = splitTree yt
splitTree :: Avl a -> (a, Avl a)
splitTree (Fork h xt y zt) =
    if isEmpty xt then (y,zt) else (u,rebalance vt y zt)
       where (u,vt) = splitTree xt

bias :: Avl a -> Int
bias (Fork h xt y zt) = ht xt - ht zt

rotr :: Avl a -> Avl a
rotr (Fork m (Fork n ut v wt) y zt) = fork ut v (fork wt y zt)

rotl :: Avl a -> Avl a
rotl (Fork m ut v (Fork n rt s tt)) = fork (fork ut v rt) s tt

rebalance :: Avl a -> a -> Avl a -> Avl a
rebalance xt y zt 
  | (hz+1 < hx) && (bias xt < 0) = rotr (fork (rotl xt) y zt)
  | (hz+1 < hx)                  = rotr (fork xt y zt)
  | (hx+1 < hz) && (0 < bias zt) = rotl (fork xt y (rotr zt))
  | (hx + 1 < hz)                = rotl (fork xt y zt)
  | otherwise                    = fork xt y zt
      where hx = ht xt
            hz = ht zt


