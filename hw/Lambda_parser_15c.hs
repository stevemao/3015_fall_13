module Lambda_parser where

 import Parser
 import Term

 keywords = ["lambda"]


 pAbs :: Parser Term
 pAbs = failure -- replace this code with your own

 pV :: Parser Term
 pV = failure -- replace this code with your own.

 pParenTerm :: Parser Term
 pParenTerm = failure -- replace this code with your own

 pTerm1 :: Parser Term
 pTerm1 = failure -- replace this code with your own
 

 pTerm :: Parser Term
 pTerm = do ts <- many pTerm1
            return (foldl1  Ap ts)
