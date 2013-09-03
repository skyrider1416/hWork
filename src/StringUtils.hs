-- String Utilities
-- m.j.m. December 2012

-- s `startsWith` prefix 
startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (x:xs) (y:ys) = (x == y) && startsWith xs ys

-- s `endsWith` suffix
endsWith :: String -> String -> Bool
endsWith s suffix = startsWith (reverse s) (reverse suffix)
