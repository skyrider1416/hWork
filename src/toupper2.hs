import System.IO
import Data.Char

main = do 
        end <- isEOF
        putStrLn (if end then "end" else "not end")
        linein <- getLine
        putStrLn (map toUpper linein)
        main