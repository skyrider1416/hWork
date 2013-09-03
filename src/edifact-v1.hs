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

uih1 = "SCRIPT:010:005:ERROR"
uih = "UIH+SCRIPT:010:005:ERROR+RxRefNum+PON12345ABCD++20081101:102345"

segments :: String -> [String]
segments message = splitWith (== segSep) message
    where segSep = message !! 8

elements :: String -> [String]
elements message = splitWith (=='+') message

components :: String -> [String]
components message = splitWith (==':') message

repeats :: String -> [String]
repeats message = splitWith (=='*') message

xElement :: String -> String -> String
xElement tag inner = "<" ++ tag ++ ">" ++ inner ++ "</" ++ tag ++ ">"

fComponent :: Int -> [String] -> String
fComponent _ [] = ""
fComponent n (x:xs) = xElement ("uih.1." ++ show n) x ++ fComponent (n+1) xs

fElement :: [String] -> String
fElement [] = ""
fElement (x:xs) = (xElement x) (fElement' x 1 xs)
    where
    fElement' :: String -> Integer -> [String] -> String
    fElement' _ _ [] = ""
    fElement' x n (y:ys) = xElement (x ++ "." ++ show n) (g y) ++ fElement' x (n+1) ys
    g y = fComponent 1 (components y)

tag :: String -> Integer -> Integer -> String
tag x 0 _ = x
tag x n 0 = x ++ "." ++ show n
tag x n m = (tag x n 0) ++ "." ++ show m
--parseEdi :: String -> [[[String]]]
--parseEdi message = map components (map elements (segments message))

nTag :: String -> Integer -> String
nTag name k = name ++ "." ++ show k

qTag :: String -> [String] -> String
qTag _ [] = ""
qTag s x = qqTag s 1 x
    where
    qqTag :: String -> Integer -> [String] -> String
    qqTag _ _ [] = ""
    qqTag s n (y:ys) = qqqTag s n y ++ qqTag s (n+1) ys
        where
        qqqTag :: String -> Integer -> String -> String
        qqqTag s n y = xElement (nTag s n) y
