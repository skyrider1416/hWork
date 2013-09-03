peek :: Maybe Int -> Int
peek (Just n) = n
peek Nothing = 0

peek' :: Maybe String -> String
peek' x = 
	case x of
		Just z -> z
		Nothing -> ""
		
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v
