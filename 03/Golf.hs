module Golf where

pickElem :: Int->[a]->[(Int,a)]
pickElem n = filter (\(num,_) -> num `mod` (n+1) == 0) . zip [1..]

skips :: [a] -> [[a]]
skips x = [ s | (_,s) <- [unzip (pickElem n x) | n <- [0..length x - 1]]]
