module Golf where

import           Data.Char
import          Data.List

myCount :: String -> Int
myCount = length . filter (not . isSpace)

-- Exercise 1
{-  pickup every nth elem in [a] to form a new list
    The whole process can be done as follows: first
    drop n elements from [a], and then take 1, and
    then repeat this step to the reset of the list.
-}
p :: Int->[a]->[a]
p n s = case drop n s of
        []  -> []
        x:y -> x:p n y

skips :: [a] -> [[a]]
skips x = [p n x | n <- [0..length x - 1]]

--Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima = l

{-Obviously the result is [] if the length of [a] is less than 3;
  then just go through the list according to the definetion.
-}
l :: [Integer] -> [Integer]
l s
    | length s < 3 = []
l (x:y:z:q)
    | y > x && y > z = y:l (y:z:q)
    | otherwise = l $ y:z:q

-- Exercise 3
--count how many element n is in list a.
c :: Eq a => a -> [a] -> Int
c n = length . filter (==n)

-- creat a string with first nth elements are '*' and  total length is m
t :: Int -> Int -> String
t n m = take (m-n) (cycle " ") ++ take n (cycle "*") ++ "="

histogram :: [Integer] -> String
histogram s = unlines $ transpose (map (`t` m) r) ++ [['0'..'9']]
    where r = [c n s | n <- [0..9]]
          m = maximum r
