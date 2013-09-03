f :: Integer -> String
f n 
	| n == 2 = two
	| n == 3 = three
	| otherwise = other
  where two = "two"
        three = "three"
        other = "other"
  
spell :: Int -> String
spell n
	| n >= hundred = "large " ++ spell (div n 100)
	| n >= ten = "foo " ++ spell (mod n 10)
	| otherwise = "bar"
	where 
 ten = 10
 hundred = 100