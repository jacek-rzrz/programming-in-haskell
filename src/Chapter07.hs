-- Higher-order functions

module Chapter07 where

import Data.Char

type Bit = Int
bit2int :: [Bit] -> Int
bit2int bs = foldl (\a k -> 2 * a + k) 0 bs

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = (int2bin (n `div` 2)) ++ [n `mod` 2]

make8 :: [Bit] -> [Bit]
make8 bs = if length bs < 8 then make8 (0 : bs) else bs

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = (take 8 bs) : (chop8 (drop 8 bs))

decode :: [Bit] -> String
decode = map (chr . bit2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode


-- exercises

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

dec2int :: [Int] -> Int
dec2int = foldl (\s i -> 10 * s + i) 0

curry :: ((a, b) -> c) -> a -> b -> c
curry f = (\x y -> f (x, y))

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = (\(x,y) -> f x y)