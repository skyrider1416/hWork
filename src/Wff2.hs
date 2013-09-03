
data WFF = Var Char | And WFF WFF | Or WFF WFF | Implies WFF WFF | Equivalent WFF WFF | Not WFF deriving Show

eval :: WFF -> [Bool] -> Bool
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

polish :: WFF -> String
polish (Var x) = [x]
polish (Not w) = "N" ++ polish w
polish (And w1 w2) = "K" ++ polish w1 ++ polish w2
polish (Or w1 w2) = "A" ++ polish w1 ++ polish w2
polish (Implies w1 w2) = "C" ++ polish w1 ++ polish w2
polish (Equivalent w1 w2) = "E" ++ polish w1 ++ polish w2

parse :: String -> Maybe WFF
parse s = parse' (reverse s) []
	where
		parse' :: String -> [WFF] -> Maybe WFF
		parse' [] [] = Nothing
		parse' [] (w1:w2:ws) = Nothing
		parse' [] (wff:[]) = Just wff
		parse' (x:xs) stack
			| x `elem` "pqrst" = parse' xs ((Var x):stack)
			| otherwise = case x of
				'N' -> 
					let (w1:stack') = stack
					    in parse' xs $ (Not w1) : stack'
				'K' ->
					let (w1:w2:stack') = stack
						in parse' xs $ (And w1 w2) : stack'
				'A' ->
					let	(w1:w2:stack') = stack
						in parse' xs $ (Or w1 w2) : stack'
				'C' ->
					let (w1:w2:stack') = stack
						in parse' xs $ (Implies w1 w2) : stack'
				'E' ->
					let (w1:w2:stack') = stack
						in parse' xs $ (Equivalent w1 w2) : stack'

unwrap :: Maybe a -> a
unwrap Nothing = error "unwrap Nothing"
unwrap (Just x) = x

evalM :: Maybe WFF -> [Bool] -> Maybe Bool
evalM jwff vals = do
	wff <- jwff
	let e = eval wff vals
	return e
	
	