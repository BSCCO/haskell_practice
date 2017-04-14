{-# OPTIONS_GHC -Wall #-}
module My_high_order where

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
fun1 :: [Integer] -> Integer
fun1 = product. map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate collatz
  where collatz x = if even x then div x 2 else 3 * x + 1

--Exercise 2: Folding with trees
data Tree a = Leaf
        | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = updateDepth . foldr insertTree Leaf

--insert a node into a tree by insert it into the child which has least nodes.
insertTree :: a -> Tree a -> Tree a
insertTree x Leaf                   = Node 0 Leaf x Leaf
insertTree x (Node n l a r)
        | countNode l < countNode r = Node n (insertTree x l) a r
        | otherwise = Node n l a (insertTree x r)

--count a tree has how many nodes
countNode :: Tree t -> Integer
countNode Leaf           = 0
countNode (Node _ l _ r) = 1 + countNode l + countNode r

--update the deepth number in every node
updateDepth :: Tree a -> Tree a
updateDepth (Node _ l a r) = Node (1 + max (deepth l) (deepth r)) (updateDepth l) a (updateDepth r)
updateDepth Leaf = Leaf

--tells the deepth of a tree
deepth :: Tree t -> Integer
deepth Leaf           = -1
deepth (Node _ l _ r) = 1 + max (deepth l) (deepth r)

--Exercise 3: More folds!
{-1. Implement a function
xor :: [Bool] -> Bool
which returns True if and only if there are an odd number of True
values contained in the input list. It does not matter how many
False values the input list contains.
Your solution must be implemented using a fold.
-}
xor :: [Bool] -> Bool
xor = foldr boolXor False . filter (== True)

boolXor :: Bool -> Bool -> Bool
boolXor a b = a /= b

{-
2. Implement map as a fold. That is, complete the definition
map’ :: (a -> b) -> [a] -> [b]
map’ f = foldr ...
in such a way that map’ behaves identically to the standard map
function.
-}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x->(f x:)) []

{-
3. (Optional) Implement foldl using foldr . That is, complete the
definition
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ...
in such a way that myFoldl behaves identically to the standard
foldl function.
-}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr step id xs base
    where step x g a = g (f a x)

{-
Exercise 4: Finding primes
Read about the Sieve of Sundaram. Implement the algorithm us-
ing function composition. Given an integer n, your function should
generate all the odd prime numbers up to 2n + 2.
-}
sieveSundaram :: Integer -> [Integer]
sieveSundaram = 
