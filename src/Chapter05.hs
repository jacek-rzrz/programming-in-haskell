-- List comprehensions

module Chapter05 where

import Data.Char

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (i, x') <- zip [0..length xs] xs, x == x']

c2i :: Char -> Int
c2i c = ord c -  ord 'a'

i2c :: Int -> Char
i2c i = chr (ord 'a' + i)

shift :: Int -> Char -> Char
shift n c | isLower(c) = i2c ((c2i c + n) `mod` 26)
          | isUpper(c) = shift n (toLower c)
          | otherwise  = c

encode :: Int -> String -> String
encode n cs = map (shift n) cs

dot :: [Float] -> [Float] -> Float
dot xs ys = sum [x * y | (x, y)<- zip xs ys]

count :: Eq a => a -> [a] -> Int
count a xs = length (filter (==a) xs)

hist :: String -> [Float]
hist cs = map (\i -> fromIntegral (count (i2c i) cs)) [0..25]

freqs :: [Float]
freqs = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- Score each shift (the higher score the more similarity).
-- This originated as a cosine, but since shift doesn't influence norms,
-- dot product will do just as well:
scores :: String -> [Float]
scores cs = map score hists
                where
                    score = dot freqs
                    hists = map hist shifts
                    shifts = map (\i -> encode i cs) [0..25]

crack :: String -> String
crack cs = encode n cs
            where
                n = head (positions m ss)
                m  = maximum ss
                ss = scores cs


sumSquares :: Int -> Int
sumSquares n = sum [i * i | i <- [1..n]]

replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x+y*y==z*z]

factors :: Int -> [Int]
factors n = [i | i <- [1..n], n `mod` i == 0]

perfects :: Int -> [Int]
perfects n = [i | i <- [1..n], sum (factors i) == 2 * i]