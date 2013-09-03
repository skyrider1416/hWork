-- Debuggable Functions

type Debuggble a = a -> (a, String)

bind :: (a -> (a, String)) -> ((a, String) -> (a, String))
bind f (gx, gs) = let (fx,fs) = f gx in (fx, gs ++ fs)

f :: Float -> (Float, String)
f x = (2*x, "f was called.")

g:: Float -> (Float, String)
g x = (3*x, "g was called.")


(***) :: (a -> (a, String)) -> (a -> (a, String)) -> (a -> (a, String))
f' *** g' = bind f' . g'