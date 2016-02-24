module DNA where
import qualified Data.Map as Map
import Data.Maybe()
import Data.List()

count :: Char -> String -> Int
count n xs =
	if elem n "ACGT" then
		foldr (\x y ->
			if x /= n then
				if elem x "ACGT" then
					y
				else error ("invalid nucleotide '" ++ [x] ++ "'")
			else y + 1) 0 xs
	else
		error ("invalid nucleotide '" ++ [n] ++ "'")

nucleotideCounts :: String -> Map.Map Char Int
nucleotideCounts xs = foldr (
	\x m -> case x of
		'A' -> Map.update (\v -> Just (v+1)) 'A' m
		'C' -> Map.update (\v -> Just (v+1)) 'C' m
		'G' -> Map.update (\v -> Just (v+1)) 'G' m
		'T' -> Map.update (\v -> Just (v+1)) 'T' m
		_ -> error ("invalid nucleotide '" ++ [x] ++ "'")) (Map.fromList [('A',0),('C',0),('G',0),('T',0)]) xs