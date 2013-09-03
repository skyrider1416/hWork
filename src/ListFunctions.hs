module ListFunctions
( length'
, isPrefixOf'
, isSubstringOf'
, elem'
, subsetOf'
, zipWith'
, splitWith'
, words'
, takeWhile'
, dropWhile'
, nub'
, nub''
, reverse'
, reverse''
, span'
, map'
) where

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

isPrefixOf' :: Eq a => [a]->[a]->Bool

isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = (x == y) && isPrefixOf' xs ys

-- isSubstringOf' x y is true iff x is a substring of y
isSubstringOf' :: Eq a => [a] -> [a] -> Bool
isSubstringOf' [] _ = True
isSubstringOf' _ [] = False
isSubstringOf' x yy@(y:ys) = isPrefixOf' x yy || isSubstringOf' x ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

subsetOf' :: Eq a => [a] -> [a] -> Bool
subsetOf' [] _ = True
subsetOf' _ [] = False
subsetOf' (x:xs) y = x `elem'` y && subsetOf' xs y

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' op (x:xs) (y:ys) = (x `op` y) : zipWith' op xs ys

splitWith' :: (a->Bool) -> [a] -> [[a]]
splitWith' f [] = []
splitWith' f x = pre : (splitWith' f suf')
  where (pre, suf) = break f x
        suf' = dropWhile' f suf

words' :: String -> [String]
words' x = splitWith' (==' ') x

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' pred [] = []
takeWhile' pred (x:xs)
           | pred x = x : takeWhile' pred xs
           | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' pred [] = []
dropWhile' pred (x:xs)
           | pred x = dropWhile' pred xs
           | otherwise = (x:xs)

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) 
	| x `elem` xs = nub' xs
	| otherwise = x : nub' xs

nub'' :: Eq a => [a] -> [a]
nub'' = foldr step []
	where step x xs = if x `elem` xs then xs else x : xs
	
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

reverse'' :: Eq a => [a] -> [a]
reverse'' = foldl (flip (:)) []
	
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' p xx@(x:xs) 
	| p x = let (y1, y2) = span p xs in (x:y1, y2) 
	| otherwise = ([], xx)
	
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' eq (x:xs) =
	(x : ys) : (groupBy' eq zs)
	where
		(ys, zs) = span' (eq x) xs
		
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

		