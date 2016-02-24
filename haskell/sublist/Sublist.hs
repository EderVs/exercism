module Sublist (Sublist(Equal, Sublist, Superlist, Unequal), sublist) where

data Sublist = Equal | Unequal | Sublist | Superlist deriving (Eq, Show)

sublist' :: Eq a => [a] -> Int -> [a] -> Int -> Bool
sublist' xs xs_n ys ys_n
	| xs_n > ys_n = False
	| xs == take xs_n ys = True
	| otherwise = sublist' xs xs_n (tail ys) (ys_n-1)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist xs ys 
	| xs == ys = Equal
	| sublist' xs (length xs) ys (length ys) = Sublist
	| sublist' ys (length ys) xs (length xs) = Superlist
	| otherwise = Unequal