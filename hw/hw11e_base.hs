module BTree where

data BTree a = Leaf | Node a (BTree a) (BTree a) deriving (Eq,Show)

instance Functor BTree where
  fmap f Leaf = error "your code here"
  fmap f (Node x lt rt) = error "your code here"



balanced Leaf = True   
balanced (Node _ lt rt) = (abs ((height lt) - (height rt)) <= 1) && balanced lt && balanced rt 
  where height Leaf = 0
        height (Node _ lt rt) = 1 + max (height lt) (height rt)  


mktree [] = Leaf
mktree (x:xs) = Node x (mktree ll) (mktree rl)
  where (ll,rl) = splitAt (div (length xs) 2) xs

foldrBTree f e Leaf = error "your code here"
foldrBTree f e (Node x lt rt) = error "your code here"

