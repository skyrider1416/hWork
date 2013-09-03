-- roman numerals
roman :: Integer -> String
roman n
    | n >= 1000 = 'm' : roman (n-1000)
    | n >= 900 = 'c' : roman (n+100)
    | n >= 500 = 'd' : roman (n-500)
    | n >= 400 = 'c' : roman (n+100)
    | n >= 100 = 'c' : roman (n-100)
    | n >= 90 = 'x' : roman (n+10)
    | n >= 50 = 'l' : roman (n-50)
    | n >= 40 = 'x' : roman (n+10)
    | n >= 10 = 'x' : roman (n-10)
    | n >= 9 = 'i' : roman (n+1)
    | n >= 5 = 'v' : roman (n-5)
    | n >= 4 = 'i' : roman (n+1)
    | n >= 1 = 'i' : roman (n-1)
    | otherwise = ""

-- value of a roman numeral
romval :: String -> Integer
romval ('m' : rest) = 1000 + romval rest
romval ('c' : 'm' : rest) = 900 + romval rest
romval ('d' : rest) = 500 + romval rest
romval ('c' : 'd' : rest) = 400 + romval rest
romval ('c' : rest) = 100 + romval rest
romval ('x' : 'c' : rest) = 90 + romval rest
romval ('l' : rest) = 50 + romval rest
romval ('x' : 'l' : rest) = 40 + romval rest
romval ('x' : rest) = 10 + romval rest
romval ('i' : 'x' : rest) = 9 + romval rest
romval ('v' : rest) = 5 + romval rest
romval ('i' : 'v' : rest) = 4 + romval rest
romval ('i' : rest) = 1 + romval rest
romval _ = 0