main = putStrLn (show (take 20 fib))
fib = 0:1:zipWith (+) fib (tail fib)
