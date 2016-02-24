module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear x | (x `mod` 4) == 0 && (not ((x `mod` 100) == 0) || (x `mod` 400) == 0) = True
			 | otherwise = False