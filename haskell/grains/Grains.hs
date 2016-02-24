module Grains (square, total) where

square :: Integral a => Int -> a
square n = round (2^^(n-1))

total :: Integral a => a
total = foldr (\x y -> (y+(square x))) 0 [1..64]