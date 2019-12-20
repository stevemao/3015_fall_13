module Main where
  
  import Parser
  import Type_inference

  ioParse :: String -> IO Term
  ioParse  s = 
     let (t,s') = head (apply termP s) in
       if s' == "" then 
         return t 
       else 
         fail "parse error"

  ioInfer  :: Term  -> IO ([(String,Type)],Type)
  ioInfer t = return (infer t)

  typecheck = 
    do putStr "enter_term: "
       ln <- getLine
       if ln == ""   then
           do putStr "good bye.\n"
              return ()
         else 
           do trm <- ioParse  ln
              (context,typ) <- ioInfer trm
              putChar '\n'
              putStr ((show context) ++ " |- " ++ (show trm) ++ " :: " ++ (show typ))
              putChar '\n'
              typecheck

       