primes = sieve [2..]

-- sieve :: Num a => [a] -> [a]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]


goldbach ::  Integer -> [(Integer, Integer)]
goldbach n = [(j,k) | j <- takeWhile (<n) primes, k <- takeWhile (<n) primes, j <= k, j+k == n]

primePairs :: Integer -> [(Integer, Integer)]
primePairs n = [(j,k) | j <- takeWhile (< n) primes, k <- takeWhile (< n) primes]

