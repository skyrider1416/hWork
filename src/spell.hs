-- Spell out the integer in words (simple version)
spell n
    | n < 20 = units !! n
    | n < 100 = (decades !! k ) ++ " " ++ spell (mod n 10)
    | n < 1000 = spell (div n 100) ++ " hundred " ++ spell (mod n 100)
    | n < 1000000 = spell (div n 1000) ++ " thousand " ++ spell (mod n 1000)
    | otherwise = spell (div n 1000000) ++ " million " ++ spell (mod n 1000000)
  where 
     k = div n 10
     units=["","one","two","three","four","five","six","seven","eight","nine","ten",
            "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
     decades = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]