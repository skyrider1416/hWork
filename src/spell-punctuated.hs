-- Spell out the integer in words (slightly more complicated version, because it does the punctuation, such as hyphenating "sixty-two"
spell' n
    | n < 20 = units !! n
    | n < 100 = (decades !! div n 10) ++ hyphen (mod n 10) ++ spell' (mod n 10)
    | n < 1000 = spell' (div n 100) ++ " hundred" ++ space (mod n 100) ++ spell' (mod n 100)
    | n < 1000000 = spell' (div n 1000) ++ " thousand" ++ space (mod n 1000) ++ spell' (mod n 1000)
    | otherwise = spell' (div n 1000000) ++ " million" ++ space (mod n 1000000) ++ spell' (mod n 1000000)
  where
     units=["","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
     decades = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
     hyphen x = if x == 0 then "" else "-"
     space x = if x == 0 then "" else " "
