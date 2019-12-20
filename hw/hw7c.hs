{- Base code for HW 7 -}
data Nat = Zero | Succ Nat
   deriving (Eq,Ord,Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = nat2int n + 1
 

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat (k + 1) = Succ ( int2nat k)

bottom = bottom

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Zero = c
foldn h c (Succ n) = h (foldn h c n)

shownat Zero = "Zero"
shownat (Succ k) = "Succ(" ++ shownat k ++ ")"

wrap left right s = left ++ s ++ right

id_nat n = foldn Succ Zero n
plus n m = foldn Succ n m
times n m = foldn (plus n) Zero m
expt n m = foldn (times n) (Succ Zero) m
