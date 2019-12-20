module Lambda_parser where

 import Parser



 keywords = ["lambda"]

 data Term = V String | Ap Term Term | Abs String Term deriving (Eq,Show)

 -- The Ap parser
 pAp :: Parser Term
 pAp = failure -- replace this code with your own

 -- The Abs parser (denoted by the keyword "lambda" in the concrete grammar)
 pAbs :: Parser Term
 pAbs = failure -- replace this code with your own

 -- The variable parser - you might want to disallow variables named "lambda"
 pV :: Parser Term
 pV = failure -- replace this code with your own.

 -- The parser for a parenthesis enclosed term
 pParenTerm :: Parser Term
 pParenTerm = failure -- replace this code with your own

 -- put all the term1 parsers together here
 pTerm1 :: Parser Term
 pTerm1 = failure -- replace this code with your own
 
 -- I've implemented this one for you.
 pTerm :: Parser Term
 pTerm = do ts <- many pTerm1
            return (foldl1  Ap ts)
