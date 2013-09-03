-- Parser (using Maybe datatype)
-- my experimental version -- m. j. merchant

module Parser 
( Parser (..)
, anyChar
, pzero
, sat
, char
, string
, regexp
, space
, number
, token
, (<|>)
, many
, many1
) where 

import Control.Monad
import Text.Regex.TDFA

data Parser a = Parser { parse :: String -> Maybe(a, String)}
instance Monad Parser where
    return x = Parser (\s -> (Just (x,s)) )
    p >>= f = Parser (\cs -> case  parse p cs of
                    Nothing -> Nothing
                    Just (x, cs') -> parse (f x) cs') 

-- match any character
anyChar :: Parser Char
anyChar = Parser (\cs -> case cs of
                    "" -> Nothing
                    (c:cs) -> Just (c, cs) )

-- zero Parser always fails
pzero :: Parser ()
pzero = Parser (\x -> Nothing)

-- matches one character if the predicate is true for that character
sat :: (Char -> Bool) -> Parser Char
sat pred = Parser(\cs -> case cs of
                    "" -> Nothing
                    (c:cs') -> if pred c then Just(c, cs') else Nothing
                    )

-- matches the given character
char :: Char -> Parser Char
char c = sat (==c)

-- matches the given string
string :: String -> Parser String
string s = Parser (\cs -> if (startsWith s cs) then Just(s, drop (length s) cs) else Nothing )
	where
		startsWith :: String -> String -> Bool
		startsWith [] _ = True
		startsWith _ [] = False
		startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

-- matches the given regular expression
-- Uses POSIX type regular expressions
-- the regex should be anchored at the beginning of line (^)		
regexp :: String -> Parser String
regexp pat = Parser (g)
            where g :: String -> Maybe (String, String)
                  g cs
                    | cs =~ pat :: Bool = let (_, s, cs') = cs =~ pat :: (String,String,String)
                                          in Just (s, cs')
                    | otherwise = Nothing

-- matches zero or more spaces
space = regexp "^[ \t]*"
 
-- matches an integer or floating point number   
number = regexp "^[0-9]+([.][0-9]*)?([eE][+-]?[0-9]+)?"

-- p <|> q - matches p or q. If p matches, just match p
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\cs -> let w = parse p cs in case w of
                            Nothing -> parse q cs
                            _ -> w )

-- token p matches p followed by space
token :: Parser a -> Parser a
token p = do{x <- p; space; return x;}

-- matches zero or more instance of p
many :: Parser a -> Parser [a]
many p =  many1 p <|> return []

-- matches one or more instances of p
many1 ::  Parser a -> Parser [a]
many1 p = do {x <- p; y <- many p; return (x:y)}
