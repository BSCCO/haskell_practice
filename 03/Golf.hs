module Golf where

import           Data.Char
import          Data.List

count :: String -> Int
count = length . filter (not . isSpace)

--pickup every nth elem in [a] to form a new list
p :: Int->[a]->[a]
p n s = case drop n s of
        []  -> []
        x:y -> x:p n y

skips :: [a] -> [[a]]
skips x = [p n x | n <- [0..length x - 1]]

localMaxima :: [Integer] -> [Integer]
localMaxima = l

l :: [Integer] -> [Integer]
l s
    | length s < 3 = []
l (x:y:z:q)
    | y > x && y > z = y:l (y:z:q)
    | otherwise = l $ y:z:q


--count how many element n is in list a.
c :: Eq a => a -> [a] -> Int
c n = length . filter (==n)

-- creat a string with first nth elements are '*' and  total length is m
t :: Int -> Int -> String
t n m = take (m-n) (cycle " ") ++ take n (cycle "*")

histogram :: [Integer] -> String
histogram s = unlines $ transpose [t (c n s) m | n <- [0..9]] ++ ["==========", "0123456789"]
    where m = maximum [c n s | n <- [0..9]]
