qsort [] = []
qsort (h:t) = qsort lesser ++ [h] ++ qsort greater
  where
        lesser = [y | y <- t, y < h]
        greater = [y | y <- t, y >= h]
