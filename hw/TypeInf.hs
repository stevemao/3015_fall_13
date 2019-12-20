module TypeInf where

import Data.List        
import Term
import UnificationPair
        
fresh :: String -> [String] -> String
fresh x xs = if not(x `elem` xs) then x else fresh' x 0  
   where fresh' x i  = 
            if not(x' `elem` xs) then x' else fresh' x (i + 1) 
                where x' = x ++ (show i)
 
type Context = [(String,Type)]
 
infer :: Context -> Term -> Type -> [String] -> ([Constraint],[String])
 
infer ctx  (Var x) ty vars = 
   case (lookup x ctx) of
      Just t -> ([(ty,t)],vars)
      Nothing -> error "infer: Var-case  failure"
 
infer ctx (Ap t1 t2) ty vars = (c1 ++ c2, vars2)
    where (c1,vars1) = infer ctx t1 (BinType Arrow (TVar a) ty) (a : vars)
          (c2,vars2) = infer ctx t2 (TVar a) vars1
          a =  fresh "a" vars
 
infer ctx (Abs x t) ty vars = ((BinType Arrow (TVar a) (TVar b), ty) : c, vars')
   where (c,vars') = infer ((x, TVar a) : ctx) t (TVar b) (a : b : vars)
         a = fresh "a" vars
         b = fresh "b" vars

--- you need to extend the defintion of the infer function for the Pair, Fst and Snd constructors.
-- Add your code 

 
test_infer t = infer [] t (TVar "a") ["a"]
 

typeof t = pretty (subst s (TVar "a"))
     where (c,_) = infer [] t (TVar "a") ["a"]
           s = unifyL c

-- the following function cleans up the type variable names in a type. 
pretty ty = if toolong then ty else subst abc ty
      where abc = (\x -> TVar ((toEnum (97 + (unjust (elemIndex x vs))) :: Char) : []))
            vs = vars ty
            unjust (Just k) = k
            toolong = length vs > 25
        
-- typeof (Abs "x" (Var "x"))
-- typeof (Ap (Abs "x" (Var "x"))(Abs "x" (Var "x")))
-- typeof (Abs "x" (Abs "y" (Ap (Var "y") (Var "x"))))
-- typeof (Abs "x" (Abs "y" (Abs "z" (Ap (Ap (Var "x") (Var "z"))(Ap (Var "y") (Var "z"))))))
-- typeof (Pair (Abs "x" (Var "x")) (Abs "x" (Var "x")))
-- typeof (Fst (Pair (Abs "x" (Var "x")) (Abs "x" (Var "x"))))
-- typeof (Fst (Pair (Abs "x" (Abs "y" (Ap (Var "y") (Var "x")))) (Abs "x" (Var "x"))))
-- typeof (Snd (Pair (Abs "x" (Abs "y" (Ap (Var "y") (Var "x")))) (Abs "x" (Var "x"))))
-- typeof (Snd (Pair (Abs "x" (Var "x")) (Abs "x" (Abs "y" (Ap (Var "y") (Var "x"))))))
