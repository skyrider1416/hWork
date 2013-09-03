-- polymorphism in Haskell
myFunc :: Int -> Int 
myFunc x = x+1

myFunc :: String -> String
myFunc s = s ++ s