import Mjm.Combin
import Test.QuickCheck

hello x = "hello " ++ x

newtype PosInt a = PosInt a deriving (Show)
    
instance (Integral a, Num a, Arbitrary a) => Arbitrary (PosInt a) where
	arbitrary = do
		a <- arbitrary
		return $ if (a == 0) then PosInt 1 else PosInt (abs a)
		
		
prop_permutations (PosInt x) (PosInt y) = (nPk n k) == (permutations n k)
    where k = y
          n = x + k

prop_combinations (PosInt x) (PosInt y) = (nCk n k) == (combinations n k)
    where k = y
          n = x + k

prop_fact_1 n = (n >= 1) ==> (factorial n) == (product [1..n])