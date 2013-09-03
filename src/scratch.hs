	         
parseWff :: Parser Wff
parseWff = Parser (\cs -> case cs of
					(c:cs) -> case c of
							'p' -> (Var 'p', cs)
							'K' -> do
									w1 <- parseWff
									w2 <- parseWff
									return (And w1 w2)
							'A' -> do
									w1 <- parseWff
									w2 <- parseWff
									return (Or w1 w2)
							'N' -> do
									w1 <- parseWff
									return (Not w1)
							'C' -> do
									w1 <- parseWff
									w2 <- parseWff
									return (Implies w1 w2)
							'E' -> do
									w1 <- parseWff
									w2 <- parseWff
									return (Equivalent w1 w2)
				)