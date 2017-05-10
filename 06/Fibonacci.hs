{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

fibs2 :: [Integer]
fibs2 = [a | (a,_,_) <- iterate next (0,1,1)]
    where next (_,b,c) = (b,c,b+c)

data Stream a = a `Cons` (Stream a)

streamToList :: Stream a -> [a]
streamToList (y `Cons` xs) = y : streamToList xs

instance Show a => Show (Stream a) where
    show s = show . take 20 $ streamToList s

streamRepeat :: a -> Stream a
streamRepeat s = s `Cons` streamRepeat s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (y `Cons` xs) = f y `Cons` streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = b `Cons` streamFromSeed f b
    where b = f a

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap largestDividesOf2 nats

largestDividesOf2 :: Integer -> Integer
largestDividesOf2 n = snd $ largestPowerOf2 (n,0)

largestPowerOf2 :: Integral a => (a,a)->(a,a)
largestPowerOf2 (a,b)
        | y == 0 = largestPowerOf2 (z, b+1)
        | otherwise = (a, b)
        where (z,y) = quotRem a 2

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (z `Cons` xs) (y `Cons` ys) = z `Cons` (y `Cons` interleaveStreams xs ys)

x :: Stream Integer
x = 0 `Cons` (1 `Cons` streamRepeat 0)

instance Num (Stream Integer) where
        fromInteger n = n `Cons` streamRepeat 0
        negate = streamMap negate
        (m `Cons` ms) + (n `Cons` ns) = (m + n) `Cons` (ms + ns)
        (m `Cons` ms) * b@(n `Cons` ns) = (m * n) `Cons` (streamMap (m*) ns + ms * b)

instance Fractional (Stream Integer) where
        (m `Cons` ms) / (n `Cons` ns) = (m `div` n) `Cons` streamMap (`div` n) (ms -  ((m `Cons` ms) / (n `Cons` ns)) * ns)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
        (Matrix a b c d) * (Matrix m n z y) = Matrix (a*m+b*z) (a*n+b*y) (c*m+d*z) (c*n+d*y)

fib4 :: Integer -> Integer
fib4 n = f where (Matrix _ f _ _) = Matrix 1 1 1 0 ^ n
