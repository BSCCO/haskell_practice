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
        |n<=0=False
        |otherwise=sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n _ _ _
        |n <= 0 = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
