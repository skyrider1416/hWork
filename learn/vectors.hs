
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

head' :: [a] -> a
head' [] = error "empty list"
head'(x:_) = x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:y) = (reverse' y) ++ [x]
