module Golf where

count :: String -> Int
count = length . filter (\c -> c `notElem` [' ', '\t'])

--pickup every nth elem in [a] to form a new list
p :: Int->[a]->[a]
p n s = case drop n s of
        [] -> []
        x:y -> x:p n y

skips :: [a] -> [[a]]
skips x = [p n x | n <- [0..length x - 1]]
