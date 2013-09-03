ff n = [k | k <- [1..q]]
	where q = floor $ sqrt $ fromIntegral n

fff n = [k | k <- reverse [2..q] ]
	where q = floor $ sqrt $ fromIntegral n

-- computes (a^n) mod p, recursively and efficiently (n >= 0 an integer)
powerMod :: Integer -> Integer -> Integer -> Integer
powerMod a 0 p = 1	
powerMod a n p 
	| (n `mod` 2) == 0 = (powerMod a (n `div` 2) p)^2 `mod` p
	| otherwise			= (a * powerMod a (n-1) p) `mod` p
	
probablyPrime :: Integer -> Bool
probablyPrime p = ((powerMod 2 (p-1) p) == 1) && ((powerMod 3 (p-1) p) == 1)

ssieve (p:xs) = p : [x | x<-xs, probablyPrime x]

probablePrimes = ssieve [2..]