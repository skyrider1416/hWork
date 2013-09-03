-- Parser for WFFs, Version 5, using Parsec
-- Michael Merchant 

import Text.Parsec
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)
import Data.Char
import Data.Maybe
import Data.Either

data Wff = Var Char | And Wff Wff | Or Wff Wff | Implies Wff Wff | Equivalent Wff Wff | Not Wff deriving Show

-- evaluate a Wff, giving it a list of pairs,
-- where each pair is a variable name and its truth value
-- example: eval wff (zip "pq" [True, False])
-- evaluates the given wff with Variable p = True, and Variable q = False

eval :: Wff -> [(Char, Bool)] -> Bool
eval (Var c) [] = error "variable not found"
eval var@(Var c) ((name,truth) : xs)
            | c == name = truth
            | otherwise = eval var xs
      
eval wff xs = case wff of
    And w1 w2 -> (eval w1 xs) && (eval w2 xs)
    Or w1 w2 ->(eval w1 xs) || (eval w2 xs)
    Implies w1 w2 -> 
        let (v1,v2) = (eval w1 xs, eval w2 xs) in
            case (v1,v2) of
                (True,False) -> False
                _ -> True
    Equivalent w1 w2 -> (eval w1 xs) == (eval w2 xs)
    Not w -> not (eval w xs)

-- convert a WFF to Polish notation string
polish :: Wff -> String
polish wff = case wff of
    Var x -> [x]
    Not w -> "N" ++ polish w
    And w1 w2 -> "K" ++ polish w1 ++ polish w2
    Or w1 w2 -> "A" ++ polish w1 ++ polish w2
    Implies w1 w2 -> "C" ++ polish w1 ++ polish w2
    Equivalent w1 w2 -> "E" ++ polish w1 ++ polish w2

-- parsers for WFF components
pAnd = do {char 'K'; w1 <- wff; w2 <- wff; return (And w1 w2);}
pOr = do {char 'A'; w1 <- wff; w2 <- wff; return (Or w1 w2);}
pImplies = do {char 'C'; w1 <- wff; w2 <- wff; return (Implies w1 w2)}
pEquivalent = do {char 'E'; w1 <- wff; w2 <- wff; return (Equivalent w1 w2)}
pNot = do {char 'N'; w <- wff; return (Not w)}
pVar :: Parser Wff
pVar = do {v <- lower; return (Var v)}

-- parser for Wff
wff = pVar <|> pAnd <|> pOr <|> pImplies <|> pEquivalent <|> pNot

-- here is an alternative technique, defining an equivalent Wff parser
wff' :: Parser Wff
wff' = 
    do {v <- lower; return (Var v)} <|>
    do
        c <- letter
        case c of
            'K' -> do {w1 <- wff'; w2 <- wff'; return (And w1 w2)}
            'A' -> do {w1 <- wff'; w2 <- wff'; return (Or w1 w2)}
            'C' -> do {w1 <- wff'; w2 <- wff'; return (Implies w1 w2)}
            'E' -> do {w1 <- wff'; w2 <- wff'; return (Equivalent w1 w2)}
            'N' -> do {w1 <- wff'; return (Not w1)}
            _ -> unexpected "letter" <?> "one of: KACEN"

left :: Either a b -> a
left (Left x) = x

right :: Either a b -> b
right (Right w) = w