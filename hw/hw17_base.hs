data Btree a = Nil | Node a (Btree a) (Btree a) deriving (Eq,Ord,Show)

mkBTree [] = Nil
mkBTree (h:t) = Node h (mkBTree t1) (mkBTree t2) 
  where (t1,t2) = splitAt (div (length t) 2) t

type Queue a = ([a],[a])

mkQueue () = ([],[])

push x (m,n) = (x:m,n)

top ([],[]) = error "head: empty queue."
top (m,h:t) = h
top (m,[]) = top ([],reverse m)

pop ([],[]) = error "pop: empty queue."
pop (m, h:t) = (m,t)
pop (m,[]) = pop ([], reverse m)

emptyq ([],[]) = True
emptyq (m,n) = False


 -- breadthfirst t = 



