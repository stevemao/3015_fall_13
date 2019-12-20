module Boolean where 

  type Boolean = Either () ()

  true, false :: Boolean
  true = Left ()
  false = Right ()
 
  ifthenelse :: Boolean -> a -> a  -> a
  ifthenelse b e1 e2 = either (\() -> e1) (\() -> e2) b


--   (/\),(\/),(.=>) :: Boolean -> Boolean -> Boolean
