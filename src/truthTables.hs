infix 1 ==>
(==>) :: Bool -> Bool -> Bool
(==>) p q = (not p) || q

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 phi = and [phi p q | p <- [True,False], q <- [True,False]]

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 phi = and [phi p q r | p <- tf, q <- tf, r <- tf] where tf = [True,False]

phi :: Bool -> Bool -> Bool
phi p q = (not p || q) <=> not (p && not q)