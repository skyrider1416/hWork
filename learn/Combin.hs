module Mjm.Combin 
( factorial
, combinations
, permutations
, nPk
, nCk
, elementwise
, scalar
, (.+.)
) where

factorial :: Integer -> Integer
factorial n | n < 0 = error "argument error"
factorial 0 = 1
factorial n = n*factorial (n-1)

combinations :: Integer -> Integer -> Integer
combinations _ k | k < 0 = error "argument error"
combinations _ 0 = 1
combinations n k | (k > n || n < 0) = 0 
                 | otherwise = combinations (n-1) k + combinations (n-1) (k-1)  

permutations :: Integer -> Integer -> Integer
permutations n k | k < 0 = error "argument error"
permutations _ 0 = 1
permutations n k = n * permutations (n-1) (k-1)

nPk :: Integer -> Integer -> Integer
nPk n k = (factorial n) `div` factorial (n-k)

nCk :: Integer -> Integer -> Integer
nCk n k = (factorial n) `div` (factorial k * factorial (n-k))

infixl 7 .*.
(.*.) :: (Num a) => a -> [a] -> [a]
(.*.) x y = scalar (*) x y

infixl 6 .+.
(.+.) ::  (Num a) => [a] -> [a] -> [a]
x .+. y = elementwise (+) x y

elementwise :: (a -> b -> c) -> [a] -> [b] -> [c]
elementwise op x y = [a `op` b | (a,b) <- zip x y]

scalar :: (a -> b -> c) -> a -> [b] -> [c]
scalar op a xs = [a `op` y | y <- xs]

pascal :: [[Integer]]
pascal = [1] : map f pascal
    where
	f x = 1 : zipWith (+) x (tail x) ++ [1]

sub :: (Eq b, Num b) => [a] -> b -> a
sub [] _ = error "arg error"
sub (x:xs) 0 = x
sub (x:xs) n = sub xs (n-1)

prop_pascal1 :: Integer -> Integer -> Bool
prop_pascal1 n k = (pascal `sub` n `sub` k ) == (nCk n k)