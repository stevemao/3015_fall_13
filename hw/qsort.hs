qsort [] = []
qsort (h:hs) =  qsort smaller ++ [h] ++ qsort larger_eq
   where smaller = [ x | x <- hs, x < h]
         larger_eq = [y | y <- hs, y >= h]


words [] =  []
words (x:xs) = 
