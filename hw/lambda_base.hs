module Lambda where

data Term = V String 
         | Ap Term Term 
         | Abs String Term 
	 | Pair Term Term
         | Fst Term
         | Snd Term
  deriving (Eq)

instance Show Term where
  show (V x) = x
  show (Ap m n) = "(" ++ show m  ++ " " ++ show n ++ ")"
  show (Abs x m) = "(\\" ++ x ++ "->" ++ show m ++ ")"
  show (Pair m n) = "<" ++ show m ++ "," ++ show n ++ ">"
  show (Fst m) = "fst(" ++ show m ++ ")"
  show (Snd m) = "snd(" ++ show m ++ ")"


fv (V s) = [s]
fv (Ap m n) = fv m ++ fv n
fv (Abs x m) =   filter (/=x) (fv m)
fv (Pair m n) = fv m ++ fv n
fv (Fst m) = fv m
fv (Snd m) = fv m

fresh x m = 
   if x `elem` m then
     fresh (x ++ "'") m
   else
     x

subst (y, n) (V s) = if y == s then n else (V s)
subst (x, n) (Ap m k) = Ap (subst (x, n) m) (subst (x, n) k)
subst (x, n) (Abs y m) = 
    if x == y  then
       Abs y m
    else
       if y `elem` (fv n) then
          Abs z (subst (x,n) (subst (y, V z) m))
       else
          Abs y (subst (x,n) m)
  where z = fresh y (x : y : ((fv n) ++ (fv m)))
subst (x,n) (Pair m1 m2) = Pair (subst (x,n) m1) (subst (x,n) m2)
subst (x,n) (Fst m) =  Fst (subst (x,n) m)
subst (x,n) (Snd m) =  Snd (subst (x,n) m)

             
 

test_subst (x,n) t = "subst (" ++ x ++ ", " ++ show n ++ ") " ++ show t ++ "  -->  " ++ show (subst (x,n) t)

beta (Ap (Abs x m) n) = subst (x,n) m
beta t = t

test f t = (show t ) ++ " ---> " ++ (show (f t))

higher f t = 
  let t' = f t in
    if t' /= t then
      t'
    else
      case t of
        (V x) -> t
        (Ap t1 t2) ->  Ap (higher f t1) (higher f t2)
        (Abs x t1) -> Abs x (higher f t1)

fixpoint f t = 
  let t' = f t in
    if t' == t then
      t'
    else fixpoint f t'

