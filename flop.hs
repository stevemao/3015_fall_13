flop f (x,y) = f (y,x)

flip f x y = f y x

curry f x y = f (x,y)

uncurry f (x,y) =  f x y

plus (x,y) = x + y

minus (x,y) = x - y

times (x,y) = x * y

(f . g) x = f ( g x)
