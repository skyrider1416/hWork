sieve :: Num a => [a] -> [a]
sieve [] = []
sieve (p:xs) = p : filter (`mod` p > 0) (sieve xs)