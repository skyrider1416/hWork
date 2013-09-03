fibs  = 0 : 1 : zipWith (+) fibs (tail fibs )

fibn 0 = 0
fibn 1 = 1
fibn n = fibn (n-1) + fibn (n-2)

