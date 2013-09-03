fact :: Integer -> Integer
fact n
	| n < 0 = error "argument negative"
	| n == 0 = 1
	| otherwise = n * fact(n-1)
	
fib n 
	| n < 0 = error "arg"
	| n == 0 = 0
	| n == 1 = 1
	| otherwise = fib(n-1) + fib(n-2)
	
fibvec :: [Int] -> [Int]
fibvec [] = [1,0]
fibvec [x] = [1,0]
fibvec (x:y:z) = (x+y) : x:y:z

fib' :: Int -> Int -> Int -> Int
fib' 1 b c = b
fib' a b c = fib' (a-1) (b+c) b

fibo :: Int -> Int
fibo n = fib' n 1 1 
