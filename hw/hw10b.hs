 -- Finite Functions from a to b are modelled  by the type Set (a,b) 

data FinFun a b = FF [(a,b)]
 
 -- We display finite functions as {a->b,c->d,e->f} by instantiating the show function as follows:

instance (Show a, Show b) => Show (FinFun a b) where
    show (FF m) = wrap "{" "}" (comma_sep (map (\s -> concat (map (\c -> if c == ',' then "->" else [c]) s)) (map show m)))
      where wrap c1 c2 m = c1 ++ m ++ c2
            comma_sep  = foldr1 ((++) . (++ ",")) 




 -- if you only use update to build  finite functions, they will be functional

update (x,y) (FF m) = FF [] -- replace "FF []" with your code.

 
apply (FF m) x = Nothing -- replace "Nothing" with your code.

domain (FF  m) = []  -- replace "[]" with your code
range (FF m) = [] -- replace "[]" with your code

-- test a list of pairs to see if it is functional (i.e. if each domain element appears at most once.)
functional  m =  False -- replace "False" with you code.


invert (FF m) = FF (map swap  m)
  where swap (x,y) = (y,x)

c2i = FF (zip ['a'..'z'] (map fromEnum ['a'..'z']))
i2c = invert c2i
number = FF [(1,"one"), (2, "two")]
