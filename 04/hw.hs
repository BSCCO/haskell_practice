module My_high_order where

import           Data.List

--exercise 1
{-
1. fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
        | even x = (x - 2) * fun1 xs
        | otherwise = fun1 xs

2. fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n ‘div‘ 2)
       | otherwise = fun2 (3 * n + 1)
-}
f1 :: [Integer] -> Integer
f1 = product. map (+ (-2)) . filter even
