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
data Message = Message {messageType :: MessageType, segments :: [Segment]}  deriving (Show)
data Segment = Segment {name :: String, elements :: [Element]}     deriving (Show)
data Element = Element {position :: Int, components :: [Component]} deriving (Show)
data Component = Component String | Repeats [String] deriving (Show,Eq)

--fElement :: Element -> String
--fElement (Element _ []) = ""
--fElement (Element _ x) = intercalate "+" (map fComponent x)

--fComponent :: Component -> String
--fComponent (Component c) =  c

e1 = Element 1 [Component "aa", Component "bb"] :: Element
e2 = Element 2 [Component "cc", Component "dd"] :: Element
s1 = Segment "XYZ" [e1, e2] :: Segment
m1 = Message ERROR [s1]

parseMessage :: String -> Message
parseMessage s = Message (extractMessageType s) (map segment (split '\'' s))
    where
    segment :: String -> Segment
    segment s = Segment {name = take 3 s, elements = map element (zip [1..] (split '+' (drop 3 s)))}

    element :: (Int, String) -> Element
    element (n, s) = Element (n :: Int) (map component (split ':' s))

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

xmlTag :: String -> String -> String
xmlTag _ "" = ""
xmlTag tagName inner = "<"++ tagName ++ ">" ++ inner ++ "</" ++ tagName ++ ">"

ediXml :: Message -> String
ediXml m = xmlTag (show $ messageType m) innerXml
    where
    innerXml = concat (map xmlSegment (segments m))
        where
        xmlSegment :: Segment -> String
        xmlSegment s = xmlTag  (name s) innerXml
            where
                innerXml = concat $ map (xmlElement $ name s) (elements s)

        xmlElement :: String -> Element -> String
        xmlElement segname element = xmlTag (segname ++ "." ++ show (position element)) "foo"

        point :: String -> Int -> String
        point s n = s ++ "." ++ (show n)