module ETL (transform) where
import qualified Data.Map as Map
import Data.Char (toLower)

addLetter :: Maybe Int -> Int -> Maybe Int
addLetter (Just x) n = Just (x + n)
addLetter Nothing n = Just (n)

addLetters :: [String] -> Int -> Map.Map String Int -> Map.Map String Int
addLetters letters n current = foldr (\[x] y -> Map.alter (\z -> addLetter z n) [toLower x] y) current letters

transform' :: [(Int, [String])] -> Map.Map String Int -> Map.Map String Int
transform' [] current = current
transform' ((n, letters):xs) current = transform' xs (addLetters letters n current)

transform :: Map.Map Int [String] -> Map.Map String Int
transform letters = transform' (Map.toList letters) (Map.fromList [])