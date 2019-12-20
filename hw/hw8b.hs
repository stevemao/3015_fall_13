last' [] = error "last': no last' in []."
last' (x:xs) = if null xs then x else last' xs

ismem  x [] = False 
ismem x (y:ys) = x == y || ismem x ys

allaremem [] _ = True
allaremem (x:xs) ys = ismem x ys && allaremem xs ys

find x [] = error "find: not found."
find x ((y,z): yzs) = if x == y then z else find x yzs

