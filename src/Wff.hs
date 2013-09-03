
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

newtype Parser a = Parser (String -> (a, String)) 

parse :: Parser a -> (String -> (a, String))
parse (Parser p) = p

instance Monad Parser where
    return a = Parser (\s -> (a, s))
    p >>= f = Parser( \cs -> let (a, cs') = parse p cs
                             in parse (f a) cs' )
             
parseW :: String -> (Wff, String)
parseW (c:cs) | c `elem` "pqrstuv" = (Var c, cs)
              | otherwise = case c of
                    'N' -> let (w1, cs') = parseW cs
                           in (Not w1, cs')
                    'K' -> let (w1, cs') =  parseW cs
                               (w2, cs'') = parseW cs'
                           in (And w1 w2, cs'')
                    'A' -> let (w1, cs') =  parseW cs
                               (w2, cs'') = parseW cs'
                           in (Or w1 w2, cs'')
                    'C' -> let (w1, cs') = parseW cs
                               (w2, cs'') = parseW cs'
                           in (Implies w1 w2, cs'')
                    'E' -> let (w1, cs') = parseW cs
                               (w2, cs'') = parseW cs'
                           in (Equivalent w1 w2, cs'')          