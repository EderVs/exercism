module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

isMultOfAny :: Int -> [Int] -> Bool
isMultOfAny _ [] = False
isMultOfAny y (x:xs) = if y `mod` x == 0 then True else isMultOfAny y xs

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples multiples n = foldr (\x y -> if isMultOfAny x multiples then y + x else y) 0 [1..(n-1)]

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault n = sumOfMultiples [3,5] n