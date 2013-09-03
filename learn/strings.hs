delim :: String -> String
delim ('U':'N':'A':y)= "UNA SEGMENT" ++ y
delim ('U':'N':'B':y) = "UNB SEGMENT" ++ y
delim z = "whatever " ++ z

reverse' :: String -> String
reverse' [] = []
reverse' (x:xs) = (reverse xs) ++ [x]