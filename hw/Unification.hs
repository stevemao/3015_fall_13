module Unification where

import Data.List
import Data.Char

data  Term =   Var String | Abs  (String,Term) | Ap Term Term  | Pair Term Term | Fst Term | Snd Term
        deriving (Eq,Show)
        
data Op = Arrow | Product deriving (Eq)


data  Type =   TVar  String |  BinType Op  Type   Type
        deriving (Eq)
        
instance Show Type where
   show (TVar x) = x
   show (BinType Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
   show (BinType Product t1 t2) = "(" ++ show t1 ++ " X " ++ show t2 ++ ")"

type Substitution = String -> Type

idSubst :: Substitution
idSubst x = TVar x

update :: (String, Type) -> Substitution -> Substitution
update (x,y) f = (\z -> if z == x then y else f z)


-- vars collects all the variables occuring in a type expression
vars :: Type -> [String]
vars ty = nub (vars' ty) 
    where vars' (TVar x) = [x]
          vars' (BinType op t1 t2) = vars' t1 ++ vars' t2

subst :: Substitution -> Type -> Type
subst s (TVar x) = s x
subst s (BinType op t1 t2) = BinType op (subst s t1) (subst s t2)

unify :: Type -> Type -> Substitution
unify (TVar x) (TVar y) = if x == y then idSubst else update (x, TVar y) idSubst
--- implement the rest of the cases

test  t1 t2 = 
     let s = unify t1 t2 in
        do print ("s = " ++ show (map (\x -> (TVar x, subst s (TVar x))) (nub (vars t1 ++ vars t2))))
           print ("subst s " ++ show t1 ++ " = " ++ show (subst s t1))
           print ("subst s " ++ show t2 ++ " = " ++ show (subst s t2))
           return ()





-- test (TVar "a") (TVar "b")
-- test (BinType Arrow (TVar "a") (TVar "b")) (TVar "a") 
-- test (BinType Arrow (TVar "a") (TVar "b")) (TVar "b") 
-- test (BinType Arrow (TVar "a") (TVar "b")) (TVar "c") 
-- test (BinType Arrow (TVar "a") (TVar "b")) (BinType Arrow (TVar "c") ( TVar "a"))
-- test (BinType Arrow (TVar "a") (TVar "b")) (BinType Arrow (TVar "b") ( TVar "a"))
-- test (BinType Arrow (TVar "a") (TVar "b")) (BinType Arrow (TVar "b") ( BinType Arrow(TVar "a") (TVar "b")))
-- test (BinType Arrow (TVar "a") (TVar "b")) (BinType Arrow (TVar "b") ( BinType Arrow(TVar "a") (TVar "b")))
-- test (BinType Arrow (TVar "a") (TVar "b")) (BinType Arrow (TVar "b") ( BinType Arrow(TVar "a") (TVar "b")))
-- test (BinType Arrow (TVar "a") (TVar "a")) (BinType Arrow (BinType Product (TVar "b") (TVar "c")) (TVar "d"))
-- test (BinType Arrow (TVar "a") (TVar "a")) (BinType Arrow (BinType Product (TVar "b") (TVar "c")) (TVar "d"))
-- test (BinType Arrow (TVar "a") (TVar "a")) (BinType Arrow (BinType Product (TVar "b") (TVar "c")) (TVar "c"))
-- test (BinType Product (TVar "a") (TVar "b"))  (BinType Product (TVar "b") (TVar "c")) 
