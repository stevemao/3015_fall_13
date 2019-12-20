divides d n = rem n d == 0

data Formula = Bottom | Var String | Fand (Formula, Formula)

valuation a Bottom = True
valuation a  (Var x) = a x
valuation a (Fand (phi, psi))  = (valuation a phi) && (valuation a psi)


compose f g x = f(g x)

uncurry f  = (\(x,y) -> f x y)
curry f  = (\x -> \y -> f(x,y))


