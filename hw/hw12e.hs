module Calculator where

data BinOp = Plus | Minus | Times | Div | And | Or  deriving Show

meaning Plus = (+)
meaning Minus = (\x y -> x - y)
meaning Times = (*)
meaning Div = div
meaning And = 0 -- <- your code here (remember an Int is false if it is (==0) and true otherwise)
meaning Or = 0 -- <- your code here

data Exp = Const Int | Var String | BinExp BinOp Exp Exp   | Let String Exp Exp | If Exp Exp Exp | FF | Not Exp 
   deriving Show 

type Assignment = (String -> Int)
update :: (String,Int) -> Assignment -> Assignment
update (x,v) f = (\y -> if x == y then v else f y)

a0 :: Assignment
a0 x = error ("undefined variable " ++ x)

eval :: Assignment -> Exp  -> Int
eval a (Const k) = k
eval a (Var s) = a s
eval a (BinExp op e1 e2) = (meaning op) (eval a e1) (eval  a e2)
eval a (Let x e1 e2) = 0 -- <- your code here
eval a (If b e1 e2) = 0 -- <- your code here 
eval a FF = 0 -- <- your code here
eval a (Not e) = 0 -- <- your code here


