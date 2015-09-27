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

