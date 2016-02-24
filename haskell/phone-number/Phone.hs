module Phone (areaCode, number, prettyPrint) where
import Data.Char (isDigit)

areaCode :: String -> String
areaCode phone = take 3 phone

number :: String -> String
number phone
	| n == 11 && (head clean_phone) == '1' = tail clean_phone
	| n == 10 = clean_phone
	| otherwise = "0000000000"
	where
		(clean_phone, n) = foldr (\x (y,z) -> if isDigit x then ((x:y),(z+1)) else (y,z)) ("",0::Int) phone

prettyPrint :: String -> String
prettyPrint phone = '(':(areaCode clean_phone)++") "++(take 3 (drop 3 clean_phone))++"-"++(take 4 (drop 6 clean_phone))
	where
		clean_phone = (number phone)