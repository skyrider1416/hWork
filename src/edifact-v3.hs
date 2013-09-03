import Data.List

splitWith :: (a->Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f x = pre : (splitWith f suf')
  where (pre, suf) = break f x
        suf' = dropWhile f suf

split:: Char -> String -> [String]
split c s = splitWith (==c) s

msg = "UNA:+. *'" ++
    "UIB+UNOA:0++somenewmsgid:RelatestoMessageID+++9999999999001:D:senderpwd:sendertid+9999999:P:recippwd:reciptid+20081101:102345++::::::::::::::::::::::::::::::::::0'" ++
    "UIH+SCRIPT:010:005:ERROR+RxRefNum+PON12345ABCD++20081101:102345'" ++
    "STS+900+660*661*662*663*664*665*666*667*668*669+abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij'" ++
    "UIT+PON12345ABCD+3'" ++
    "UIZ++1'"

data MessageType = STATUS | ERROR | NEWRX | REFREQ | REFRES | CHGRES | RXFILL | VERIFY | RXHREQ | RXHRES | ELGREQ | CANRX | CANRES   deriving (Show,Eq,Ord)
data Message = Message MessageType [Segment]  deriving (Show)
data Segment = Segment String [Element]    deriving (Show)
data Element = Element [Component] deriving (Show)
data Component = Component String | Repeats [String] deriving (Show,Eq)

--fElement :: Element -> String
--fElement (Element _ []) = ""
--fElement (Element _ x) = intercalate "+" (map fComponent x)

--fComponent :: Component -> String
--fComponent (Component c) =  c

e1 = Element [Component "aa", Component "bb"] :: Element
e2 = Element [Component "cc", Component "dd"] :: Element
s1 = Segment "XYZ" [e1, e2] :: Segment
m1 = Message ERROR [s1]

parseMessage :: String -> Message
parseMessage s = Message (extractMessageType s) (map segment (split '\'' s))
    where
    segment :: String -> Segment
    segment s = Segment (take 3 s) (map element (split '+' (drop 3 s)))

    element :: String -> Element
    element s = Element (map Component (split ':' s))

    component :: String ->  Component
    component s = fcomponent (split '*' s)
        where
            fcomponent :: [String] -> Component
            fcomponent [] = Component ""
            fcomponent (x : []) = Component x
            fcomponent y = Repeats y

    extractMessageType :: String -> MessageType
    extractMessageType msg =
        let
            u = (split '\'' msg) !! 2
            v = (split '+' u) !! 1
            w = (split ':' v) !! 3
        in parseMessageType w

    parseMessageType :: String -> MessageType
    parseMessageType s =
        case s of
            "STATUS" -> STATUS
            "ERROR" -> ERROR
            "NEWRX" -> NEWRX
            "REFRES" -> REFRES
            "REFREQ" -> REFREQ
            "CHGRES" -> CHGRES
            "RXFILL" -> RXFILL
            "VERIFY" -> VERIFY
            "RXHREQ" -> RXHREQ
            "RXHRES" -> RXHRES
            "CANRX" -> CANRX

tag :: String -> String -> String
tag _ "" = ""
tag tagName inner = "<"++ tagName ++ ">" ++ inner ++ "</" ++ tagName ++ ">"

nTag :: String -> Int -> String
nTag s n = s ++ "." ++ (show n)

tag2:: String -> Int -> String -> String
tag2 tagName num inner = tag (nTag tagName num) inner

tag3 :: String -> Int -> Int -> String -> String
tag3 tagName i j inner = tag (nTag (nTag tagName i) j) inner

ediXml :: Message -> String
ediXml (Message m segments) = tag (show m) (foldl step "" segments)
    where
    step :: String -> Segment -> String
    step zero segment = zero ++ (xmlSegment segment)

    xmlSegment :: Segment -> String
    xmlSegment (Segment name elements) = tag name (foldl element "" (zip [1..] elements))
        where
        element z (n, e) = z ++ tag2 name n (foldl comcomponents
            where
            components
            xmlElement :: Element -> String
            xmlElement (Element components) =
            tag (name ++ "." ++ (show n)) (foldl stepp "" (zip [1..] components))
                where
                stepp zero (n, c) = "foo"

        xmlSegment :: Segment -> String
        xmlSegment (Segment name elements) = tag name (xmlElements name 1 elements)
            where
                xmlElements :: String -> Int -> [Element] -> String
                xmlElements _ _ [] = ""
                xmlElements s n (x:xs) = (tag (s ++ "." ++ (show n))) (xmlElement s n x) ++ xmlElements s (n+1) xs

        xmlElement :: String -> Int -> Element -> String
        xmlElement seg n (Element _) = tag (seg ++ "." ++ (show n)) "element"

        point :: String -> Int -> String
        point s n = s ++ "." ++ (show n)

-- test
states = ["Minnesota", "Wisconsin", "Iowa"]
x = ["Papa Bear", "linda", "Krissy", "Nessa", "Beelers", "Tai"]

ff :: Show a => String -> Int -> Int -> a -> String
ff tagName i j x = tag (tagName ++ "." ++ (show i) ++ "." ++ (show j)) (show x)
