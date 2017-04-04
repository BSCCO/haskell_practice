module Golf where

import Data.Char

count :: String -> Int
count = length . filter (not . isSpace)

--pickup every nth elem in [a] to form a new list
p :: Int->[a]->[a]
p n s = case drop n s of
        [] -> []
        x:y -> x:p n y

skips :: [a] -> [[a]]
skips x = [p n x | n <- [0..length x - 1]]

localMaxima :: [Integer] -> [Integer]
localMaxima [] =[]
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x:y:z:q)
    | y > x && y > z = y:localMaxima (y:z:q)
    | otherwise = localMaxima (y:z:q)
