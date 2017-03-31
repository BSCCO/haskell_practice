module Golf where

pickElem :: Int->[a]->[a]
pickElem n s = case xs of
        []->[]
        x:xss -> x:pickElem n xss
        where xs = drop n s

skips :: [a] -> [[a]]
skips x = [ pickElem n x | n <- [0..length x - 1]]
