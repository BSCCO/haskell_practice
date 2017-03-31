import           Test.QuickCheck

toDigits :: Integer -> [Integer]
toDigits n
        |n <= 0 = []
        |otherwise = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
        |n <= 0 = []
        |otherwise = (n `mod` 10):toDigitsRev (n `div` 10)

doubleEveryOtherPos :: [Integer] -> [Integer]
doubleEveryOtherPos []         = []
doubleEveryOtherPos [x]        = [x]
doubleEveryOtherPos (x1:x2:xs) = x1:(x2*2):doubleEveryOtherPos xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherPos (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n
        |n <= 0 = False
        |otherwise = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n _ _ _
        |n <= 0 = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

{-hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg [Move]
hanoi2 n _ _ _ _
        |n <= 0 = []
hanoi2 n a b c d = oneToTwo (n-1) a c d b ++ [(a,b)] ++ twoToOne (n-1) c d b a

--move a to bc
oneToTwo :: Integer -> Peg -> Peg -> Peg -> Peg [Move]
oneToTwo n _ _ _ _
	|n <= 0 = []
oneToTwo n a b c d = oneToTwo (n-1) a c d b ++ [(a,b)] ++ twoToOne (n-1) c d b a

--move ab to c
twoToOne :: Integer -> Peg -> Peg -> Peg -> Peg [Move]
twoToOne n _ _ _ _
	|n <= 0 = []
twoToOne n a b c d = -}
