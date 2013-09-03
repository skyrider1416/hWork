import Data.List
import
msg = "UNA:+. *'" ++
    "UIB+UNOA:0++somenewmsgid:RelatestoMessageID+++9999999999001:D:senderpwd:sendertid+9999999:P:recippwd:reciptid+20081101:102345++::::::::::::::::::::::::::::::::::0'" ++
    "UIH+SCRIPT:010:005:ERROR+RxRefNum+PON12345ABCD++20081101:102345'" ++
    "STS+900+660*661*662*663*664*665*666*667*668*669+abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij'" ++
    "UIT+PON12345ABCD+3'" ++
    "UIZ++1'"

data Message = Message [Segment]  deriving (Show)
data Segment = Segment {name ::String, elements::[Element]}     deriving (Show)
data Element = Element {position :: Integer, components :: [Component]} deriving (Show)
data Component = Component String deriving (Show,Eq)

fElement :: Element -> String
fElement (Element _ []) = ""
fElement (Element _ x) = intercalate "+" (map fComponent x)

fComponent :: Component -> String
fComponent (Component c) =  c

e1 = Element 1[Component "aa", Component "bb"]     :: Element
e2 = Element 2 [Component "cc", Component "dd"] :: Element
s1 = Segment "XYZ"[e1, e2] :: Segment
m1 = Message [s1] :: Message

parseMessage :: String -> Message
parseMessage s = Message map segment ((splitWith (=='\'') s))

segment :: String -> Segment
segment s = Segment {name = take 3 s, elements = map element (splitWith (=='+') (drop 3 s))}

element :: String -> Element
element s = Element 1 s