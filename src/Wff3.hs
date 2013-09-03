    
data Parser a = Parser { parse :: String -> [(a, String)] }
instance Monad Parser where
    return x = Parser (\s -> [(x,s)] )
    p >>= f = Parser (\s -> let [(x,s')] = parse p s
                                [(y, s'')] = parse (f x) s'
                            in  [(y, s'')]
                      )


item :: Parser Char
item = Parser nxt
nxt :: String -> [(Char,String)] 
nxt [] = []
nxt (c:cs) = [(c,cs)]
data Wff = Var Char | And Wff Wff | Or Wff Wff | Implies Wff Wff | Equivalent Wff Wff | Not Wff deriving Show

eval :: Wff -> [Bool] -> Bool
eval (Var x) xs = 
    case x of
        'p' -> xs !! 0
        'q' -> xs !! 1
        'r' -> xs !! 2
        's' -> xs !! 3
        't' -> xs !! 4
        _ -> error "unrecognized variable name"
        
eval (And w1 w2) xs = (eval w1 xs) && (eval w2 xs)
eval (Or w1 w2) xs = (eval w1 xs) || (eval w2 xs)
eval (Implies w1 w2) xs = 
    let (v1,v2) = (eval w1 xs, eval w2 xs) in
        case (v1,v2) of
            (True,False) -> False
            _ -> True
            
eval (Equivalent w1 w2) xs = (eval w1 xs) == (eval w2 xs)
eval (Not w) xs = not (eval w xs)

polish :: Wff -> String
polish (Var x) = [x]
polish (Not w) = "N" ++ polish w
polish (And w1 w2) = "K" ++ polish w1 ++ polish w2
polish (Or w1 w2) = "A" ++ polish w1 ++ polish w2
polish (Implies w1 w2) = "C" ++ polish w1 ++ polish w2
polish (Equivalent w1 w2) = "E" ++ polish w1 ++ polish w2
            
parseWff :: Parser Wff
parseWff = do
    c <- item
    if c == 'N' then 
         do 
            w1 <- parseWff
            return (Not w1)
    else if c `elem` "AKCE" then 
         do
            w1 <- parseWff
            w2 <- parseWff
            case c of
                'A' -> return (Or w1 w2)
                'K' -> return (And w1 w2)
                'C' -> return (Implies w1 w2)
                'E' -> return (Equivalent w1 w2)
    else
        return (Var c)
            