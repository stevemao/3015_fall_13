module Expression where

import Parser

data BinOp = Add | Times
   deriving Show

data Exp = Const Int | BinExp BinOp Exp Exp 
   deriving Show 

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term 
             return (f * t)
            +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
           +++ natural

expr' :: Parser Exp
expr' = return (Const 0)   --    <- your code here

term' :: Parser Exp
term' =  return  (Const 0)  --    <- your code here

factor' :: Parser Exp
factor' = return (Const 0)  --    <- your code here

