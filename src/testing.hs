import ListFunctions
import Test.QuickCheck
import Data.List

prop_nubs xs = nub' xs == nub'' xs
prop_idemp_nubs xs = nub' xs == nub' (nub' xs)
prop_idemp_nubs' xs = nub' xs == (nub' . nub') xs

prop_elems x ys = elem x ys == elem' x ys

