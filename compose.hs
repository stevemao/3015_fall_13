(o) :: (b -> c) -> (a -> b) -> a -> c
f o g = \x -> f (g x)