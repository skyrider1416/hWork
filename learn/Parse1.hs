data Parser a = Parser { parse :: String -> [(a, String)] }
instance Monad Parser where
	return x = Parser (\s -> [(x,s)] )
	p >>= f = Parser (\s -> let [(x,s')] = parse p s
	                            [(y, s'')] = parse (f x) s'
	                        in  [(y, s'')]
	                  )
	
p1 :: Parser Char
p1 = Parser (\(c:cs) -> [(c,cs)])

p2 :: Parser String
p2 = do
	c1 <- p1
	c2 <- p1
	return (c1:c2:"")
	
p3 :: Parser String
p3 = do
	c <- p1
	case c of
		'a' -> return "Able"
		'b' -> return "Baker"
		
f :: Int -> String
f n = case n of
	1 -> "One"
	2 -> "Two"
	3 -> "Three"
	_ -> "Other"
	
g :: String -> String
g (c:cs) = case c of
			'a' -> "Able " ++ cs
			'b' -> "Baker " ++ cs
			_ -> "Charlie " ++ cs

p4 :: Parser Char
p4 = Parser h

h :: String -> [(Char, String)]
h (c:cs) = [(c, cs)]
h [] = []

