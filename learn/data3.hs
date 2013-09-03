import Data.List
data Expr a where
       I   :: Int  -> Expr Int
       B   :: Bool -> Expr Bool
       Add :: Expr Int -> Expr Int -> Expr Int
       Mul :: Expr Int -> Expr Int -> Expr Int
       Eq  :: Expr Int -> Expr Int -> Expr Bool
