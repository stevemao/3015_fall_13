module Tree where

import Data.Functor
import Control.Applicative
import qualified Data.Foldable  as F
import Data.Monoid


data Tree a = Leaf | Node (Tree a) a (Tree a)  deriving (Eq,Show)

mktree [] = Leaf
mktree xs = Node (mktree ls) r (mktree rs)
  where (ls,r:rs) = splitAt (length xs `div` 2) xs

-- instance Functor Tree where
  -- fill this in


-- instance Applicative Tree where
--   pure x = 
--   Leaf <*> _ = 
--   (Node l f r) <*> Leaf = 
--   (Node l f r) <*> (Node l1 x r1)  = 


-- the following implementation if right out of LYAHFGG -- but you should read it anyway.

instance F.Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Node l x r) =  F.foldMap f l `mappend` f x `mappend` F.foldMap f r

flatten :: Tree a -> [a]
flatten = F.foldr (\x xs -> x:xs) []
