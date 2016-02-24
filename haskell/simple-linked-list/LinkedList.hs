module LinkedList where

data List a = List [a] deriving Show

nil :: List a
nil = List []

isNil :: List a -> Bool
isNil (List []) = True
isNil _ = False

new :: Eq a => a -> List a -> List a
new x (List xs) = List (x:xs)

newR :: a -> List a -> List a
newR x (List xs) = List (xs++[x])

datum :: Eq a => List a -> a
datum (List []) = error "There is no element"
datum (List (x:_)) = x

next :: Eq a => List a -> List a
next (List []) = error "There is no element"
next (List (_:xs)) = List xs

toList :: Eq a => List a -> [a]
toList (List xs) = xs

fromList :: [a] -> List a
fromList xs = List xs

reverseLinkedList :: List a -> List a
reverseLinkedList (List []) = List []
reverseLinkedList (List (x:xs)) = newR x (reverseLinkedList (List xs))
