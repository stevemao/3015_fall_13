
import Data.Functor

data T a = C a | D a a  | E [a] deriving (Eq,Show)


--- instantiate the type (T a) as an instance of the Functor type class.


data Rose a = Rose a [Rose a] deriving (Eq,Show)


--- instantiate the type (Rose a) as an instance of the Functor type class.

-- some instances of rose trees for testing.
t1 = Rose 1 []
t2 = Rose 1 [Rose 2 []]
t3 = Rose  1 [Rose 2 [Rose 3 []]]
t4 = Rose  1 [Rose 2 [Rose 3 []],Rose 4 []]
t5 = Rose  1 [Rose 2 [Rose 3 []],Rose 4 [Rose 5 []]]
t6 = Rose 1 [t1,t2,t3,t4,t5]
t7 = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 []]
trees = [t1,t2,t3, t4,t5,t6,t7]

