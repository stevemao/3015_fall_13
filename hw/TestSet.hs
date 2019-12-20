module TestSet where

  import Set

  mkset :: (Ord a) => [a] -> Set a
  mkset = foldr insert empty

  ax1 x xs = insert x ( insert x xs) == insert x xs
  ax2 x y xs = insert x (insert y xs) == insert y (insert x xs)
  ax3 = isEmpty empty == True
  ax4 x xs = isEmpty (insert x xs) == False
  ax5 y = member empty y  == False
  ax6 x xs y = member (insert x xs) y == ((x==y ) || member xs y)
  ax7 x = delete x empty == empty
  ax8 x y xs = delete x (insert y xs) == if (x==y) then delete x xs else insert y (delete x xs)
  ax9 xs = union xs empty == xs
  ax10 xs y ys = union xs (insert y ys) == insert y (union xs ys)
  ax11 xs = meet xs empty == empty
  ax12 xs y ys = meet xs (insert y ys) == if (member xs y) then insert y (meet xs ys) else meet xs ys

