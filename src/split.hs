module Split
( splitWith
, split
) where

splitWith :: (a->Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f x = pre : (splitWith f suf')
  where (pre, suf) = break f x
        suf' = dropWhile f suf

split:: Char -> String -> [String]
split c s = splitWith (==c) s
