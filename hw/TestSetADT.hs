module TestSetADT where

  import SetADT

  test_S1 x = ismem x empty == False
  test_S2 x y s = ismem x (insert y s) == (x == y || ismem x s)
 
-- The following test looks a bit weird 
--   for some reason we have to tell the type checker some information about the type of empty for it to work.
 
  test_S3 =  size (empty :: Set ()) == 0   


-- add the tests for axioms S4 - S10 here

 