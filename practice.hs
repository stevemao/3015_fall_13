-- calculate the types of the following functions -- and test your answers using the Haskell interpreter.

f1 g x y = g (y,x)

f2 j k = (j,k)

f3 (x,y) g = g y x

f4 f h x = h (f x)



-- a harder one

f5 f y z = f (f z y) y


-- some types -- write functions having these types.

-- h1 :: a -> a
-- h2 :: (a -> b) -> a -> (b,a)
-- h3 :: a -> ((a,b) -> c) -> b -> c
-- h4 :: ((a,b) -> c) -> b -> a -> c
-- h5 :: (a -> b) -> [a] -> [b]
-- h6 :: [a] -> (a -> (a,b)) -> [(a,b)]
