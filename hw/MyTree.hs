module MyTree where

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

-- mktree turns a list into a realtively balanced tree
mktree :: [a] -> Tree a
mktree [] = Leaf
mktree (x:xs) = Node x (mktree left) (mktree right)
  where (left,right) = splitAt (length xs `div` 2) xs

-- flatten turns a tree into a list (inverse of mktree)
flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node x left right) = x : (flatten left ++ flatten right)

-- mapT maps a function over a tree
mapT :: (a -> b) -> Tree a -> Tree b
mapT f Leaf = Leaf
mapT f (Node x left right) = Node (f x) (mapT f left) (mapT f right)

-- foldT collapses a tree
foldT :: ( a -> b -> b -> b) -> b -> Tree a -> b
foldT f id Leaf = id
foldT f id (Node x left right) = f x (foldT f id left) (foldT f id right)


