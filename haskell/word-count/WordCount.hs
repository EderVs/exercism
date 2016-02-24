module WordCount (wordCount) where
import qualified Data.Map.Lazy as Map
import Data.Char (isAlphaNum, toLower)

addWord :: Maybe Int -> Maybe Int
addWord (Just x) = Just (x + 1)
addWord Nothing = Just (1)

words' :: String -> [String]
words' [] = []
words' text = c_text:words' next
	where
		(_, ready) = break (\x -> isAlphaNum x) text
		(c_text, n_text) = span (\x -> isAlphaNum x) ready
		(_, next) = break (\x -> isAlphaNum x) n_text

wordCount :: String -> Map.Map String Int
wordCount text = foldr (\x y -> Map.alter addWord x y) (Map.fromList []) (words' (map toLower text))