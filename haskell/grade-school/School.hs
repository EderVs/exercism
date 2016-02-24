module School where
import Data.List (sort)

type School = [(Int, [String])]

empty :: School
empty = []

add :: Int -> String -> School -> School
add n name [] = [(n, [name])]
add n name ((x_n, names):xs)
	| n == x_n = (x_n, name:names):xs
	| otherwise = (x_n, names):(add n name xs)

grade :: Int -> School -> [String]
grade _ [] = []
grade n ((x, names):xs)
	| x == n = sort names
	| otherwise = grade n xs

doMerge :: School -> School -> School
doMerge xs [] = xs
doMerge [] ys = ys
doMerge ((x, xnames):xs) ((y, ynames):ys)
	| x > y = (y, ynames):doMerge ((x, xnames):xs) ys
	| otherwise = (x, xnames):doMerge xs ((y, ynames):ys)

mergeSort :: School -> School
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = doMerge left right where
	left = mergeSort (take ((length xs) `div` 2) xs)
	right = mergeSort (drop ((length xs) `div` 2) xs)

sorted :: School -> School
sorted xs = mergeSort (map (\(x, names) -> (x, sort names)) xs)