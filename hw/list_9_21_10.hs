data Tsil a = Nil | Snoc ( Tsil a) a
  deriving (Eq,Ord,Show)


list2tsil xs =
   case xs of
    [] -> Nil
    (y:ys) -> Snoc (list2tsil ys) y



listlist2tsil [] = Nil
listlist2tsil (x:xs) = Snoc (listlist2tsil xs) (list2tsil x)


append [] xs = xs
append (y:ys) xs = y : append ys xs

myconcat [] = c
   where c = []
myconcat (xs: xss) = h xs  (myconcat xss)
   where h z y = z ++ y


myconcat1 [] = []
myconcat1 (xs: xss) = xs ++   (myconcat1 xss)


reverse1 [] =  []
reverse1 (x:xs) = h x  (reverse1 xs)
  where h z w = w ++ [z]


last [] = error "foo"
last (x:xs) = if null x then x else last xs