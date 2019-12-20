data Exp = N Int 
         | V String 
	 | Add Exp Exp 
	 | Mul Exp Exp 
	 | Let String Exp Exp 
	 | If Exp Exp Exp 
	 | Not Exp 
	 | And Exp Exp 
	 | FF 
	 | Or Exp Exp
  deriving (Eq,Show)

true x = x /= 0


eval m FF = 0
eval m (N k) = k
eval m (V x) = m x 
eval m (Add e1 e2) = (eval m e1) + (eval m e2)
eval m (Mul e1 e2) = (eval m e1) * (eval m e2)
eval m (Let x e1 e2) = eval m' e2
  where m' z = if z == x then (eval m e1) else m z
eval m (And e1 e2) = fromEnum (true (eval m e1) &&  true (eval m e2))
eval m (Or e1 e2) = fromEnum (true (eval m e1) || true (eval m e2))
eval m (If e1 e2 e3) = 1
eval m (Not e1) = 1


