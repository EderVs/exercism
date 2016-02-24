module Strain (keep, discard) where

keep :: Eq a => (a -> Bool) -> [a] -> [a]
keep f xs = foldr (\x y -> if f x then x:y else y) [] xs

discard :: Eq a => (a -> Bool) -> [a] -> [a]
discard f xs = foldr (\x y -> if f x then y else x:y) [] xs