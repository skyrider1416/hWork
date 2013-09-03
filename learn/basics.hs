myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLast :: [a] -> a
myLast (x:xs) = if null xs then x else myLast xs
myLast [] = error "empty list"

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p    [] = []
myTakeWhile p (x:xs) 
    | p x = x : myTakeWhile p xs
    | otherwise = []

myDropWhile :: (a -> Bool) ->  [a] -> [a]
myDropWhile _ [] = []
myDropWhile p y@(x:xs)
    | p x = myDropWhile p xs
    | otherwise = y


multipleChoice :: Char -> String
multipleChoice c = "foo " ++ ff c
            where 
              ff c
                | c `elem` "pqrs" = "Variable"
                | c `elem` "N" = "Unary op"
                | c `elem` "AKCV" = "Binary op"
                | otherwise = "Unknown"

myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 p xs = let (x,y) = span p xs in x

nestedIf :: Int -> Int
nestedIf n =
    if (even n) then 0 
    else if n < 10 then 1 
    else if n < 20 then 2
    else 3
    