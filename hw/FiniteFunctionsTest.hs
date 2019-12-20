module FiniteFunctionsTest where
 import FiniteFunctions

 unjust (Just k) = k
 unjust (Nothing) = error "unjust: Nothing!"

 c2i = FF (zip ['a'..'z'] [1..])
 i2c = FF (zip [1..] ['a'..'z'])

 invert (FF m) = FF (map (\(x,y) -> (y,x)) m)