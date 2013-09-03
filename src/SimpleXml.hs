-- Parser for Simple XML, using Parsec
-- Michael Merchant 
import ParsecPlus
import Text.Parsec
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec.Char
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
-- import Text.Parsec.Language (haskellStyle)
import Data.Char
import Data.Maybe
import Data.Either
import Text.Regex.TDFA

data XmlNode = TextNode {name:: String, text:: String} | ElementNode {name :: String, childNodes :: [XmlNode]} deriving (Show)

-- open xml tag - does not consume any input if does not match
peekOpenTag :: Parser ()
peekOpenTag = do
    s <- getInput
    if (s =~ "^<[a-zA-Z]" :: Bool) then return () else fail ""
 
openTag :: Parser String  
openTag = do {char '<'; s <- identifier; spaces; char '>'; return s}

closeTag :: String -> Parser ()
closeTag tagName = do{char '<'; char '/'; string tagName; spaces; char '>'; return ()} 

innerText :: Parser String
innerText = many (noneOf "<>")

textNode :: Parser XmlNode
textNode = do
    spaces
    char '<'
    tagName <- identifier
    spaces
    c <- oneOf "/>" <?> "close tag"
    case c of
        '/' -> do {char '>'; return (TextNode tagName "")}
        '>' -> do {text <- innerText; closeTag tagName; return (TextNode tagName text)}

nodeList :: Parser [XmlNode]
nodeList = many (do{peekOpenTag; t <- xmlNode; return t;})

xmlNode :: Parser XmlNode
xmlNode = (try textNode) <|> do {name <- openTag; children <- nodeList; closeTag name; return (ElementNode name children)}

doc1 = "<message>\n" ++
    "<to>michael</to>\n" ++
    "<from>Linda</from>" ++
    "<items><item>one</item><item>two</item><item><subitem>sub1</subitem></item></items></message>"