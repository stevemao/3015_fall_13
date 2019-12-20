module SetADT(Set, empty, ismem, size, insert, delete, union, intersection) where

import Data.List (nub)

newtype Set a = MkSet [a]

instance (Eq a, Show a ) => Show (Set a) where
   show (MkSet m) = "{" ++ contents ++ "}" 
     where s = show (nub m)
           contents = [s!!i | i <- [1.. length s - 2]]

instance (Eq a, Show a ) => Eq (Set a) where
    (MkSet s) == (MkSet t) = all (\x -> x `elem` s) t &&  all (\x -> x `elem` t) s 


empty = MkSet []
ismem x (MkSet m) =  x `elem` m

-- complete the implementations of the following functions
size (MkSet m) = error "size - not implemented"
insert x (MkSet m) = error "insert - not implemented"
delete x (MkSet m) = error "delete - not implemented"
union (MkSet m) (MkSet n) = error "union - not implemented"
intersection (MkSet m) (MkSet n) = error "intersection - not implemented"
