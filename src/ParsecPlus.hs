-- Some of my own parsers, using Parsec
module ParsecPlus 
( regEx
, identifier
, run
) where

import Text.Parsec
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)
import Data.Char
import Text.Regex.TDFA
import Test.HUnit

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
    Left err -> do 
                    putStr "parse error at "
                    print err
    Right x -> print x

-- Parse a string that matches a given regular expression. 
-- The regular expression should be "anchored" by beginning with a ^
regEx :: String -> Parser String
regEx pat = 
    do
        s <- getInput
        let match = s =~ pat :: String in
            if (s =~ pat :: Bool) then (string match) else fail "" <?> "regular expression: " ++ pat

identifier :: Parser String
identifier = regEx "^[A-Za-z][A-Za-z0-9]*"

ptest :: Parser (String, String)
ptest = do {x <- regEx "^[0-3]+"; y <- many alphaNum; return (x, y)}

p1 :: Parser Char
p2 :: Parser String
p1 = char 'x'
p2 = many p1

{-
bal :: Char -> Char -> Parser String
bal c1 c2 = do {w <- text; x <- char c1; y<- many (bal c1 c2); z <- char c2; return (w++[x]++y++[z]);}

text :: Parser String
text = many alphaNum     
-}
        
--test1 = TestCase(assertEqual ""  (Right ("012210", "45")) (parse ptest "" "01221045") 
--tests = TestList [TestLabel "test1" test1]

--main = runTestTT tests