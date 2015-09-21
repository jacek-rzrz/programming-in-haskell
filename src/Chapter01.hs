-- Introduction

module Chapter01 where

prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
        where
            smaller = [v | v <- xs, v < x]
            larger = [v | v <- xs, v >= x]