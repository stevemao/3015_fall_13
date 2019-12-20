module FiniteFunctions where
 
 import MoreLists
 import Sets

 -- Finite Functions from a to b are modelled  by the type Set (a,b) 

 data FinFun a b = FF [(a,b)]
 
 -- We display finite functions as {a->b,c->d,e->f}
 instance (Show a, Show b) => Show (FinFun a b) where
    show (FF m) = wrap "{" "}" (sepBy "->" "," (lexBy [',','{','}','(',')']  (show (S m)) []))
       where 
           s = show (S m) 
           lexBy sep [] [] = []
           lexBy sep [] m = [m]
           lexBy sep (x:xs) m  
              | x `elem` sep   = if m /= [] then (reverse m) : lexBy sep xs [] else lexBy sep xs []
              | otherwise      = lexBy sep xs (x:m)

           sepBy c1 c2 [] = []
           sepBy c1 c2  [x] = x
           sepBy c1 c2 [x,y] = x ++ c1 ++ y
           sepBy c1 c2 (x:y:xs) = x ++ c1 ++ y ++ c2 ++ sepBy c1 c2 xs
           sepBy c1 c2 (x:xs) = x ++ c1 ++ sepBy c1 c2 xs 
           wrap c1 c2 m = c1 ++ m ++ c2


 -- if you only use update to build  finite functions, they will be functional

--  update (x,y) (FF m) = << add your code here and uncomment >>
 
--  apply (FF m) x = << add your code here and uncomment >>

 -- the domain and range of a finite function 

--  domain (FF  m) = << add your code here and uncomment >>
--  range (FF m) = << add your code here and uncomment >>

-- test a list of pairs to see if it is functional (i.e. if each domain element appears at most once.)
--  functional  m =  << add your code here and uncomment >>


--  instance (Eq a, Eq b)  => Eq (FinFun a b) where
--   (FF m) == (FF n)   =   << add your code here and uncomment >>

 
