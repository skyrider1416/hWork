import Data.Char
main = do
       s <- getLine
       let t = map toUpper s
       putStrLn t
       main