module Nat where


data Nat = Z | S Nat deriving (Show,Eq)

mk_nat 0 = Z
mk_nat (n + 1) = S (mk_nat n)
