module FinFun(FinFun, Avl.empty, apply, update, dom, range, injection)  where

import Prelude hiding (foldr,all)
import Data.Foldable
import Avl
import FPair

data FinFun a b = FinFun (Avl (FPair a b)) 

instance (Show a, Show b) => Show (FinFun a b) where
  show (FinFun f) = show (foldr (:) [] f)

-- Fill in the code for the following functions:

-- empty :: FinFun a b
-- apply :: (Eq a,Show a) => (FinFun a b) -> a -> b
-- update :: (Ord a) => FinFun a b -> (a, b) -> FinFun a b
-- dom :: FinFun a b -> [a]
-- range :: FinFun a b -> [b]
-- injection :: (Eq a, Show a, Eq b) => FinFun a b -> Bool
