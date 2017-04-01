module Golf where

import           Data.Array

count :: String -> Int
count = length . filter (\c -> c `notElem` [' ', '\t'])

--short for length
l :: [a] -> Int
l = length

--pickup every nth elem in [a] to form a new list
p :: Int->[a]->[a]
p n x = [array (1, l x) (zip [1..] x)!m | m<-[1..l x], m `mod` (n+1) == 0]

skips :: [a] -> [[a]]
skips x = [p n x | n <- [0..l x - 1]]
