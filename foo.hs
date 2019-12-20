myflip f y x = f x y 


mklist 0 x = []
mklist k x = x :  mklist (k -1) x


p x y = x + y


data Term = Var String | Ap (Term, Term) | Abs (String, Term) deriving Show



get_one [] = error "ugh!"
get_one (x:xs) = x

strip_off_one [] = error "ughh!!"
strip_off_one (x:xs) = xs

