-- Parser for WFFs, Version 4, using my Parser module

import Parser
import Data.Char
import Data.Maybe

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

-- convert a WFF to Polish notation string
polish :: Wff -> String
polish (Var x) = [x]
polish (Not w) = "N" ++ polish w
polish (And w1 w2) = "K" ++ polish w1 ++ polish w2
polish (Or w1 w2) = "A" ++ polish w1 ++ polish w2
polish (Implies w1 w2) = "C" ++ polish w1 ++ polish w2
polish (Equivalent w1 w2) = "E" ++ polish w1 ++ polish w2

-- parsers for WFF components
pAnd = do {char 'K'; w1 <- wff; w2 <- wff; return (And w1 w2);}
pOr = do {char 'A'; w1 <- wff; w2 <- wff; return (Or w1 w2);}
pImplies = do {char 'C'; w1 <- wff; w2 <- wff; return (Implies w1 w2)}
pEquivalent = do {char 'E'; w1 <- wff; w2 <- wff; return (Equivalent w1 w2)}
pNot = do {char 'N'; w <- wff; return (Not w)}
pVar = do {v <- sat isLower; return (Var v)}

-- parser for Wff
wff = pVar <|> pAnd <|> pOr <|> pImplies <|> pEquivalent <|> pNot
        
