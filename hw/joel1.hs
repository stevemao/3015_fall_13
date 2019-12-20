
data Rose a = Node a [Rose a]
       deriving (Eq,Show)
data BTree a = Empty | Fork a (BTree a) (BTree a)
       deriving (Eq,Show)

nat (Node x [])=Fork x Empty Empty
nat (Node x [t1,t2]) = Fork x (nat t1) (nat t2 )

--tester f [x:xs]=tester f xs

gihi Empty = 0
gihi (Fork x t1 t2)=1 + gihi t1 + gihi t2

whats x = x+1