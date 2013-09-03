import Control.Monad
    
data Parser a = Parser { parse :: String -> [(a, String)] }
instance Monad Parser where
    return x = Parser (\s -> [(x,s)] )
    p >>= f = Parser (\s -> concat [parse (f x) s' | (x,s')<- parse p s])
                               


anyChar :: Parser Char
anyChar = Parser nxt
       where
       nxt [] = []
       nxt (c:cs) = [(c,cs)]

pzero :: Parser ()
pzero = Parser (\x -> [])

parse2 = do {x <- item; y<- item; return (x:y:[])}

parse2' = item >>= (\x -> (item >>= (\y -> return (x:y:[]))))

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

string :: String -> Parser String
string ss = Parser (\s -> if startsWith ss s then [(ss, drop (length ss) s)] else [])

char :: Char -> Parser Char
char c = Parser (\(x:xs) -> if x == c then [(c, xs)] else [])

anyOf :: String -> Parser Char
anyOf ss = Parser (\(c:cs) -> if c `elem` ss then [(c,cs)] else [])

notAnyOf :: String -> Parser Char
notAnyOf ss = Parser (\(c:cs) -> if not(c `elem` ss) then [(c,cs)] else [])

many :: Parser a -> Parser [a]
many p =  many1 p `mplus` return []

many1 ::  Parser a -> Parser [a]
many1 p = do {x <- p; y <- many p; return (x:y)}
	
instance MonadPlus Parser where
	mpzero = Parser (\cs -> [])
	mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

