import Data.Char
-- experiments with case statement

f :: String -> Maybe String
f "" = Nothing
f s = Just s

g :: String -> String
g s = case f s of
		Nothing -> "empty"
		Just s -> "just " ++ s
		
h :: String -> String
h s = case (s ++ "oo" )of 
		"foo" -> "it is FOO"
		"goo" -> "gooey"
		"loo" -> "fooey, gooey, and looey"
		(c:"xxoo") -> "hugs and kisses" ++ (c:[])
		_ -> "otherwise clause"
		
j :: String -> String
j "" = "empty string"
j s@(c:cs) 
	| isLower c = map toUpper s
	| otherwise = case c of
		'A' -> "OR " ++ s
		'K' -> "AND " ++s
		_ -> "Some other case"
		
-- polymorphism
class Polyglot a where
    foo :: String -> a

instance Polyglot Int where
    foo s = 47

instance Polyglot Char where 
    foo s = 'x'

instance Polyglot Double where
    foo x = 123.45

newtype TypeFoo a = TypeFoo a deriving (Show,Eq)

class NumParser a where
	parseNum :: String -> a
	
instance NumParser Int where
	parseNum x = (read x) :: Int
	
instance NumParser Double where
	parseNum x = (read x) :: Double
	

