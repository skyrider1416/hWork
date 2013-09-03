module Edifact 
( MessageType
, Message
, Segment
, Element
, Component
, parseMessage
, ediXml
) where

--import Data.List
import Split

data MessageType = STATUS | ERROR | NEWRX | REFREQ | REFRES | CHGRES | RXFILL | VERIFY | RXHREQ | RXHRES | ELGREQ | CANRX | CANRES   deriving (Show,Eq,Ord)
data Message = Message {messageType :: MessageType, segmentList :: [Segment]}  deriving (Show)
data Segment = Segment {segmentName :: String, elementList :: [Element]}    deriving (Show)
data Element = Element {componentList :: [Component]} deriving (Show)
data Component = Component String | Repeats [String] deriving (Show,Eq)

----------------------------------------------
-- parse a String into an Edifact Message

parseMessage :: String -> Message
parseMessage s = Message mt (map segment (split '\'' s))
    where
    mt = parseMessageType w
        where
        u = (split '\'' s) !! 2
        v = (split '+' u) !! 1
        w = (split ':' v) !! 3

    segment :: String -> Segment
    segment s = Segment (take 3 s) (map element (split '+' (drop 4 s)))

    element :: String -> Element
    element s = Element (map Component (split ':' s))

    component :: String ->  Component
    component s = fcomponent (split '*' s)
        where
            fcomponent :: [String] -> Component
            fcomponent [] = Component ""
            fcomponent (x : []) = Component x
            fcomponent y = Repeats y

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

----------------------------------------------
-- create Edifact XML (string) from a Message

ediXml :: Message -> String
ediXml (Message m segments) = xmlTag (show m) (xmlSegments segments)
    where
    xmlSegments :: [Segment] -> String
    xmlSegments x = concat (map xmlSegment x)

    xmlSegment :: Segment -> String
    xmlSegment (Segment name elements) = xmlTag name (xmlElements elements)
        where
        xmlElements :: [Element] -> String
        xmlElements elements = concat (map xmlElement (zip [1..] elements))

        xmlElement :: (Integer, Element) -> String
        xmlElement (n, Element components) = xmlTag elementTag (xmlComponents components)
            where
            elementTag = name ++ "." ++ (show n)

            xmlComponents :: [Component] -> String
            xmlComponents components = concat (map xmlComponent (zip [1..] components))

            xmlComponent :: (Integer, Component) -> String
            xmlComponent (j, Component cs) = xmlTag componentTag cs
                where
                componentTag = elementTag ++ "." ++ (show j)

-- some test data

msg = "UNA:+. *'" ++
    "UIB+UNOA:0++somenewmsgid:RelatestoMessageID+++9999999999001:D:senderpwd:sendertid+9999999:P:recippwd:reciptid+20081101:102345++::::::::::::::::::::::::::::::::::0'" ++
    "UIH+SCRIPT:010:005:ERROR+RxRefNum+PON12345ABCD++20081101:102345'" ++
    "STS+900+660*661*662*663*664*665*666*667*668*669+abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij'" ++
    "UIT+PON12345ABCD+3'" ++
    "UIZ++1'"

m = parseMessage msg
mx = ediXml m
