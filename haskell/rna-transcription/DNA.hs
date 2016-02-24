module DNA (toRNA) where

toRNA :: String -> String
toRNA "" = ""
toRNA (x:xs) = case x of
	'G' -> 'C':toRNA xs
	'C' -> 'G':toRNA xs
	'T' -> 'A':toRNA xs
	otherwise -> 'U':toRNA xs