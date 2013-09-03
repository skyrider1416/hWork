-- from Wadler, Philip, University of Glasgow [http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf]
-- Term is an Int or a Quotient of Ints
data Term = Con Int | Div Term Term 
	deriving (Eq, Show)
eval :: Term -> Int
eval (Con x) = x
eval (Div t u) = eval t `div` eval u

type M a = State -> (a, State)
type State = Int


unit :: a -> M a
unit a = \x -> (a,x)
compose :: M a -> (a -> M b) -> M b
m `compose` k = do
	(a, y) <- m 
	(b, z) <- k a y
	return (b, z)

instance Monad M where
	m >>= k = m `compose` k
	return = unit

eval' :: Term -> M Int
eval' (Con a) = return a
eval' (Div t u) = do
	a <- eval t
	b <- eval u
	return (a `div` b)