-- Defining functions

module Chapter04 where

halve :: [a] -> ([a], [a])
halve xs = (h1, h2)
        where
            h1 = take n xs
            h2 = drop n xs
            n = (length xs) `div` 2

safetail :: [a] -> [a]
--safetail xs = if null xs then xs else tail xs

--safetail xs
--        | null xs = xs
--        | otherwise = tail xs

safetail [] = []
safetail (x:xs) = xs

mult = (\x -> (\y -> (\z -> x * y * z)))