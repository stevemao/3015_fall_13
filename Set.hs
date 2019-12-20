module Set where

import Lists

data (Eq a, Show a) => Set a = MkSet [a]

instance (Eq a, Show a ) => Show (Set a) where
   show m = "{" ++ contents ++ "}" 
     where MkSet m' = m
           contents = reverse (drop 1 (reverse ( drop 1 (show (unique m')))))

instance (Eq a, Show a ) => Eq (Set a) where
    s == t = all ((flip elem) s') t' &&  all ((flip elem) t') s' 
               where MkSet s' = s
                     MkSet t' = t

insert x (MkSet m) = MkSet (x:m)
delete x (MkSet m) = MkSet (remove_all x m)
union (MkSet m) (MkSet n) = MkSet (m ++ n)
intersection (MkSet m) (MkSet n) = MkSet (filter (\x -> elem x n) m)
ismem x (MkSet m) = elem x m
