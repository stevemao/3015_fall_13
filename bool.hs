

data Boolean = FF | TT deriving Show

instance Eq Boolean where
   (x == y) = False
   (x /= y) = True
   