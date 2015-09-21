-- First steps

module Chapter02 where

last :: [a] -> a
last xs = xs !! (length xs - 1)
--last xs = head (reverse xs)

init :: [a] -> [a]
init xs = reverse (drop 1 (reverse xs))
--init xs = reverse (tail (reverse xs))