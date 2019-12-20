module TestSetADT where
import hw13c
    
      data Set a = Null | Fork (Set a) a (Set a) 
     
      instance Functor Set where
        fmap f Null = Null
        fmap f (Fork xt y zt)  = Fork (fmap f xt) (f y) (fmap f zt)
    
      
      foldSet f g Null = g
      foldSet f g (Fork xt y zt) = f (foldSet f g xt) y (foldSet f g zt)
    
      flattenSet = foldSet (\ys x zs -> ys ++ (x : zs)) []
    
    
      instance (Show a) => Show (Set a) where
        show s = fix (show (flattenSet s))
          where fix  = map (\ c -> case c of 
                                      '[' -> '{'
                                      ']' -> '}'
                                      _   -> c) 
    
      instance (Eq a) => Eq (Set a) where
        s1 == s2 = flattenSet s1 == flattenSet s2
        
    
      empty :: Set a
      empty = Null  
      
      isEmpty :: Set a -> Bool
      isEmpty Null = True
      isEmpty _ = False
    
      member :: (Ord a) => Set a -> a -> Bool
      member Null x = False
      member (Fork xt y zt) x  
       | (x < y) = member xt x
       | (x == y) =  True
       | (x > y) = member zt x
    
      insert :: (Ord a) => a -> Set a -> Set a
      insert x Null = Fork Null x Null
      insert x (Fork xt y zt) 
       | (x < y) = Fork (insert x xt) y zt
       | (x == y) = Fork xt y zt
       | (x > y) = Fork xt y (insert x zt)
    
      delete :: (Ord a) => a -> Set a -> Set a
      delete x Null = Null
      delete x (Fork xt y zt) 
       | (x < y) = Fork (delete x xt) y zt
       | (x == y) = join xt zt
       | (x > y) = Fork xt y (delete x zt)
    
      join :: Set a -> Set a -> Set a
      join xt yt = if isEmpty yt then xt else Fork  xt y zt
         where (y,zt) = splitTree yt
    
      splitTree :: Set a -> (a,Set a)
      splitTree (Fork xt y zt) = 
           if isEmpty xt then (y,zt) else (u,Fork vt y zt)
        where (u,vt) = splitTree xt
     
      union :: (Ord a) => Set a -> Set a -> Set a
      union s1 s2 = foldr insert s2 (flattenSet s1)
    
      -- same as intersection
      meet :: (Ord a) => Set a -> Set a -> Set a
      meet s1 s2 = foldr insert Null  (filter (member s1) (flattenSet s2) )
    
      minus :: (Ord a) => Set a -> Set a -> Set a
      minus s1 s2 = foldr insert Null  (filter (member s1) (flattenSet s2) )

