module DNA (hammingDistance) where

numOfDiff :: Eq a => [a] -> [a] -> Int -> Int
numOfDiff [] _ n = n
numOfDiff _ [] n = n
numOfDiff (x:xs) (y:ys) n = if x == y then numOfDiff xs ys n else numOfDiff xs ys (n+1)

hammingDistance :: [Char] -> [Char] -> Int
hammingDistance xs ys = numOfDiff xs ys 0
-- Using foldr and zip
--hammingDistance xs ys = foldr (\(x, y) n -> if x == y then n else (n+1)) 0 (zip xs ys)