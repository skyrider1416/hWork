data Message = Message [Segment] deriving (Show, Eq)
data Segment = Segment [Element] deriving (Show, Eq)
data Element = Element [Component]    deriving (Show, Eq)
data Component = Component String deriving (Show,Eq)

fElement :: Element -> String
fElement (Element []) = ""
fElement (Element (x:xs)) = (fComponent x) ++ "+" ++ fElement (Element xs)

fComponent :: Component -> String
fComponent (Component c) =  c