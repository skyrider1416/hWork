m = ["Linda", "Zoe", "David", "Benny"]

f :: String -> String
f s = "name: " ++ s ++ ", "

n = foldl step "" m
    where
    step zero x = zero ++ (f x)

sF :: (String -> String) -> [String] -> String
sF f x = foldl step "" x
    where
    step zero x = zero ++ (f x)