factorial :: Integer -> Integer

factorial n 
	| n < 0 = error "argument error"
	| n == 0 = 1
	| otherwise = n * factorial(n-1)