module Anagram (anagramsFor) where
import Data.List
import Data.Char

isAnagram :: String -> String -> Bool
isAnagram xs ys = (xs_l /= ys_l) && (sort xs_l) == (sort ys_l)
	where
		xs_l = map (toLower) xs
		ys_l = map (toLower) ys

anagramsFor :: String -> [String] -> [String]
anagramsFor _ [] = []
anagramsFor xs (ys:yss)
	| isAnagram xs ys = ys:anagramsFor xs yss
	| otherwise = anagramsFor xs yss