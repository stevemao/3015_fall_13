module MoreLists where

 import Data.List 


 all_pairs xs ys  = 
   do x <- xs
      y <- ys
      return (x,y)
 
 unique [] = []
 unique (x:xs) = x: (filter (/=x) xs)

 replaceStr :: String -> String -> String -> String 
 replaceStr [] old new = [] 
 replaceStr str old new = loop str 
  where loop [] = [] 
        loop str = let (prefix, rest) = splitAt n str 
                   in 
                     if old == prefix -- found an occurrence? 
                     then new ++ loop rest -- yes: replace it 
                     else head str : loop (tail str) -- no: keep looking 
        n = length old 