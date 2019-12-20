
data Rose a = Node a [Rose a] deriving (Eq,Show,Ord)
data Btree a = Empty | Fork a (Btree a) (Btree a) deriving (Eq,Show,Ord)

nat :: Rose a -> Btree a
nat (Node x []) = Empty              -- replace "Empty" with your code
nat (Node x (xt:xts)) = Empty        -- replace "Empty" with your code

inv :: Btree a -> Rose a
inv (Fork z Empty Empty) = Node 0 [] -- replace "Node 0 []" with you own code
inv (Fork z l r) = Node 0 []         -- replace "Node 0 []" with you own code



preorder_rose (Node x xts) = x : (concat (map preorder_rose xts))

preorder_Btree Empty = []
preorder_Btree (Fork x xt yt) = x : preorder_Btree xt ++ preorder_Btree yt


t1 = Node 1 []
t2 = Node 1 [Node 2 []]
t3 = Node  1 [Node 2 [Node 3 []]]
t4 = Node  1 [Node 2 [Node 3 []],Node 4 []]
t5 = Node  1 [Node 2 [Node 3 []],Node 4 [Node 5 []]]
t6 = Node 1 [t1,t2,t3,t4,t5]
t7 = Node 1 [Node 2 [], Node 3 [], Node 4 []]

trees = [t1,t2,t3, t4,t5,t6,t7]

-- if the functions are inverses then, for any rose tree t the
-- following should hold: (inv . nat) t = t and also, the preorder
-- traversals of the rose tree t should be the same as the preorder
-- traversal of the transformed btree.  You can test your code by
-- mapping the following function over the list trees.


test x = (inv . nat) x == x  && preorder_rose x == preorder_Btree (nat x)



