module FPair where

data FPair a b = P a b  

instance (Show a, Show b) => Show (FPair a b) where
  show (P x y) = show x ++ " := " ++ show y

instance (Eq a) => Eq (FPair a b) where
  (P x y) == (P z w) = x == z

instance (Ord a) => Ord (FPair a b) where
  (P x y) <= (P z w) = x <= z

