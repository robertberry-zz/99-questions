-- Problem 11

-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.

data Variable a = Multiple Int a |
                  Single a
                  deriving (Show)

pack :: Eq a => [a] -> [[a]]

pack [] = []
pack (x:xs) = (x:siblings):(pack rest)
              where (siblings, rest) = break (\y -> y /= x) xs

encode :: Eq a => [a] -> [(Int, a)]

encode xs = [(length x, head x) | x <- (pack xs)]

encode' :: Eq a => [a] -> [Variable a]
encode' = unpack . encode
          where unpack [] = []
                unpack ((times, x):xs) | times == 1 = (Single x):(unpack xs)
                                       | otherwise = (Multiple times x):(unpack xs)
