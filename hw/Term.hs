module Term where
        
import Data.List
import Data.Char

data  Term =   Var String | Abs  String Term  | Ap Term Term  | Pair Term Term | Fst Term | Snd Term
        deriving (Eq)
        
instance Show Term where
  show (Var x) = x
  show (Abs x m) = "(\\" ++ x ++ " -> " ++ (show m) ++ ")"
  show (Ap m n) = "(" ++ (show m) ++ " " ++ (show n) ++ ")"
  show (Pair m n) = "<" ++ (show m) ++ ", " ++ (show n) ++ ">"
  show (Fst m) = "fst " ++ (show m)
  show (Snd m) = "snd " ++ (show m)
