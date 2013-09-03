-- uppercase the input
import Data.Char
import System.IO

main = do
    s <- getLine
    let t = map toUpper s
    putStrLn t
    main