module Accumulate (accumulate) where

accumulate :: (Eq a, Eq b) => (a -> b) -> [a] -> [b]
accumulate _ [] = []
accumulate f (x:xs) = (f x):accumulate f xs