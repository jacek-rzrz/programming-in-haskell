-- Higher-order functions

module Chapter07 where

expressionBook f p xs = [f x | x <- xs, p x]
expressionMine f p = (map f) . (filter p)

all :: (a -> Bool) -> [a] -> Bool
all f = (foldr (&&) True) . (map f)

any :: (a -> Bool) -> [a] -> Bool
any f = (foldr (||) False) . (map f)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
			| f x = x : Chapter07.takeWhile f xs
			| otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs)
			| f x = Chapter07.dropWhile f xs
			| otherwise = (x:xs)

mymap :: (a -> b) -> [a] -> [b]
mymap f = foldr (\x xs -> (f x) : xs) []

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p = foldr (\x xs -> (h x) ++ xs) []
			where h = (\y -> if p y then [y] else [])