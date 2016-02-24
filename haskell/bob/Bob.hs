module Bob (responseFor) where
import Data.Char

numberOf :: (Char -> Bool) -> String -> Int
numberOf f text = foldr (\x y -> if f x then y + 1 else y) 0 text

responseFor :: String -> String
responseFor [] = "Fine. Be that way!"
responseFor text
	-- When all the letters are Upper case and there are 1 or more letters
	| upper_letters_number == letters_number && letters_number > 0 = "Whoa, chill out!"
	-- When the last character is ?
	| last text == '?' = "Sure."
	-- When there are at least 1 letter or 1 number
	| letters_number > 0 || numberOf isDigit text > 0 = "Whatever."
	-- Saying anything
	| otherwise = "Fine. Be that way!"
	where
		letters_number = numberOf isLetter text
		upper_letters_number = numberOf isUpper text