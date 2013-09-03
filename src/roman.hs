import Data.Char

-- roman numeral
roman :: Integer -> String
roman n = rom n vals
    where
        vals = zip [1000,900,500,400,100,90,50,40,10,9,5,4,1]
                   ["M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"]
                   
        rom :: Integer -> [(Integer, String)] -> String
        rom 0 _ = ""
        rom _ [] = ""
        rom n v = 
	        let (k, kv) = head v in
            if n >= k 
            then kv ++ rom (n-k) v
            else rom n $ tail v
            
-- numeric value of a roman numeral
romanVal :: String -> Integer
romanVal [] = 0
romanVal (x : y : rest)
    | val x < val y = romanVal (y:rest) - val x
    | otherwise = romanVal (y:rest) + val x
romanVal (x : []) = val x
    
val :: Char -> Integer
val q | isLower q = val $ toUpper q
val 'M' = 1000
val 'D' = 500
val 'C' = 100
val 'L' = 50
val 'X' = 10
val 'V' = 5
val 'I' = 1
val _ = error "not a numeral"

rvals = zip "MDCLXVI" [1000,500,100,50,10,5,1]

