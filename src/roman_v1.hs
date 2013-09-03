-- roman numeral
roman :: Integer -> String
roman 0 = ""
roman n = rom n vals
    where
        vals = [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD")
               , (100, "C"), (90, "XC"), (50, "L"), (40, "XL")
               , (10, "X"), (9, "IX"), (5, "V"), (4, "IV")
               , (1, "I") ]
        rom :: Integer -> [(Integer, String)] -> String
        rom 0 _ = ""
        rom _ [] = ""
        rom n vv@((k, kv) : kvals) = 
            if n >= k 
            then kv ++ rom (n-k) vv
            else rom n kvals

-- numeric value of a roman numeral
romanVal :: String -> Integer
romanVal [] = 0
romanVal (x : y : rest)
    | val x < val y = romanVal (y:rest) - val x
    | otherwise = romanVal (y:rest) + val x
romanVal (x : []) = val x

val :: Char -> Integer
val n
 | n == 'M' = 1000
 | n == 'D' = 500
 | n == 'C' = 100
 | n == 'L' = 50
 | n == 'X' = 10
 | n == 'V' = 5
 | n == 'I' = 1