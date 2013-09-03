-- spell out a number
spellout :: Int -> String
spellout n
	| n >= million = spellout(div n million) ++ " million " ++ spellout(mod n million)
	| n >= thousand = spellout(div n thousand) ++ " thousand " ++ spellout(mod n thousand)
	| n >= hundred = spellout(div n hundred) ++ " hundred " ++ spellout(mod n hundred)
	| n > 10 = tens !! (div n 10) ++ spellout (mod n 10)
	| otherwise = units !! n
  where
	million = 1000000
	thousand = 1000
	hundred = 100
	tens = ["","teen","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
	units = ["","one","two","three","four","five","six","seven","eight","nine"]
  
		