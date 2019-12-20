module Parser where
    import Char
    import Type_inference
 
    newtype Parser a  = MkP(String ->  [(a,String)])
    
    apply :: Parser a -> String -> [(a,String)]
    apply (MkP f) s  = f s    

    applyParser :: (Parser a) -> String -> a
    applyParser p s = 
        case (apply p s) of
          []          -> error "parse error."
          ((t,s):tss) -> t

    instance Monad Parser where

    -- (>>=) :: Parser a -> ( a -> Parser b) -> Parser b
        p >>= q = MkP f
          where f s = [(y,s'')|(x,s') <- apply p s, (y,s'') <- apply (q x) s']


--                   MkP(\i -> case (apply p i) of
--                             [] -> []
--                             [(v,out)] -> apply (f v) out)


     -- return :: a -> Parser a
        return v = MkP(\ i -> [(v,i)])

    item :: Parser Char
    item = MkP(\i -> case i of
                     [] -> []
                     (x:xs) -> [(x,xs)])
    

    zero :: Parser a
    zero = MkP(\i -> [])
    
   
                     
    p :: Parser (Char,Char)
    
    p =  do x <- item 
            item 
            y <- item
            return (x,y)

    sat :: (Char -> Bool) -> Parser Char
    sat p = do x <- item
               if p x then return x else zero

    digit  :: Parser Char
    digit  = sat isDigit
   
    digit' :: Parser Int
    digit' = do { d <- digit; return (ord d - ord '0')} 


    lower  :: Parser Char
    lower  =  sat isLower

    upper  :: Parser Char
    upper  = sat isUpper

    char      :: Char -> Parser Char
    char x = sat (==x)

    string :: String -> Parser String
    string [] = return []
    string (x:xs) = do char x
                       string xs
                       return (x:xs)

    --- combining parsers
    plus :: Parser a -> Parser a -> Parser a
    p `plus` q = MkP f 
       where f s = apply p s ++ apply q s

    orelse :: Parser a -> Parser a -> Parser a
    p `orelse` q = MkP(\i -> case apply p i of
                          [] -> apply q i
                          m -> m)

    ---  zero `plus` p = p
    ---  p `plus` zero = p
    ---  p `plus` (q `plus` r) = (p `plus` q) `plus` r
    ---  (p ++ q) >>= r = (p >>= r) `plus` (q >>= r)


    lowers :: Parser String
    lowers = do {c <- lower; cs <- lowers; return (c : cs)} `orelse` return ""
  
    letter :: Parser Char
    letter = (lower `orelse` upper)

    alphanum :: Parser Char
    alphanum = (letter `orelse` digit)


    many :: Parser a -> Parser [a]
    many p = many1 p `orelse` return []
 
    many1 :: Parser a -> Parser [a]
    many1 p = do v <- p
                 vs <- many p
                 return (v:vs)

    lowers1 :: Parser String
    lowers1 = many lower 

    ident :: Parser String
    ident = do x <- lower
               xs <- many alphanum
               return (x:xs)

    nat :: Parser Int
    nat = do xs <- many1 digit
             return (read xs)
 
    space :: Parser ()
    space = do many (char ' ')
               return ()

    token :: Parser a -> Parser a
    token p = do space
                 v <- p
                 space
                 return v

    identifier :: Parser String
    identifier = token ident

    natural :: Parser Int
    natural = token nat
  
    symbol :: String -> Parser String
    symbol xs = token (string xs)


    natlist :: Parser [Int]
    natlist = do symbol "["
                 n <- natural
                 ns <- many (do symbol ","
                                natural)
                 symbol "]"
                 return (n:ns)

    tyvarP :: Parser Type
    tyvarP = do i <- identifier
                return (TyVar i)
    
    arrowP :: Parser (Type -> Type -> Type)
    arrowP = do symbol "->"
                return Arrow
             

    prodP :: Parser (Type -> Type -> Type)
    prodP = do symbol "X"
               return Prod
             

    typeP :: Parser Type
    typeP = tyvarP `orelse` 
               do symbol "("
                  t1 <- typeP
                  op <- arrowP `orelse` prodP
                  t2 <- typeP
                  symbol ")"
                  return (op t1 t2)

    termVarP :: Parser Term
    termVarP = do i <- identifier
                  return (V i)

    pairP :: Parser Term
    pairP = return (V "pair dummy")

    spreadP :: Parser Term
    spreadP = return (V "spread dummy.")

    absP :: Parser Term
    absP = return (V "abs dummy.")

    aP :: Parser Term
    apP = return (V "apply dummy.")

    termP :: Parser Term
    termP = pairP `orelse` 
              spreadP `orelse` 
              absP `orelse`
              apP `orelse`
              termVarP 
