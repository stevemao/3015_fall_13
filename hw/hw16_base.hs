

data Ntree a = Nil | Node a (Ntree a) (Ntree a) deriving (Eq,Ord,Show)


t1 = (Node "+" (Node "*" (Node "/" (Node "a" Nil Nil) (Node "^" (Node "b" Nil Nil ) (Node "c" Nil Nil))) (Node "d" Nil Nil)) (Node "ec" Nil Nil))
t2 = (Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil))
t3 = (Node 1 (Node 2 (Node 3 Nil Nil) Nil) (Node 4 Nil Nil))
t4 = (Node 1 (Node 1 (Node 1 Nil Nil) Nil) (Node 1 Nil Nil))

index_of :: (Eq a, Num b) => a -> [a] -> b
index_of x [] = error "index_of: not found"
index_of x (h:t) = if x == h then 0 else 1 + index_of x t




