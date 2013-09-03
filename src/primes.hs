primes = sieve [2..]

-- sieve :: Num a => [a] -> [a]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

