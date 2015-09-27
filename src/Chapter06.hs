-- Recursive functions

module Chapter06 where

pow :: Int -> Int -> Int
pow i 0 = 1
pow i n = i * pow i (n-1)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && Chapter06.and xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ Chapter06.concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : Chapter06.replicate (n-1) x

nth :: Int -> [a] -> a
nth 0 (x:_) = x
nth n (_:xs) = nth (n-1) xs

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem y (x:xs) = if (x == y) then True else Chapter06.elem y xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (Prelude.take n xs, drop n xs)
			where n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge (msort part1) (msort part2)
			where (part1, part2) = halve xs

sum :: Num a => [a] -> a
sum = foldr (+) 0

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = x : Chapter06.take (n-1) xs

last :: [a] -> a
last = foldr1 (\x y -> y)