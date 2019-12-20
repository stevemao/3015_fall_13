module Sets_as_lists where

data Set a = Set [a] 

instance  (Eq a ) => Eq (Set a) where
   (Set xs) == (Set ys) =  xs `subset` ys && ys `subset` xs
     where notin xs x = not (x `elem` xs)
           subset xs ys = null (filter (notin ys) xs)


instance (Eq a, Show a) => Show (Set a) where
  show (Set xs) = fix (show (unique xs))
    where fix xs = '{' : (take (length xs -2) (drop 1 xs) ++ "}")
          unique [] = []
          unique (x:xs) = x: unique (filter (/=x) xs)

insert x (Set xs) = Set (x:xs)

elem x (Set xs) = x `Prelude.elem` xs

union (Set xs) (Set ys) = Set (xs ++ ys)

remove x (Set []) = (Set [])
remove x (Set (y:ys)) = if x == y then (Set ys) else insert x (remove x (Set ys))

