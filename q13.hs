-- Problem 13

-- Implement the so-called run-length encoding data compression method
-- directly. I.e. don't explicitly create the sublists containing the
-- duplicates, as in problem 9, but only count them. As in problem P11,
-- simplify the result list by replacing the singleton lists (1 X) by X.

import Data.Maybe

data Variable a = Multiple Int a |
                  Single a
                  deriving (Show)

encode :: Eq a => [a] -> [Variable a]

encode xs = encode' xs Nothing 0
            where encode' :: Eq a => [a] -> Maybe a -> Int -> [Variable a]
                  encode' (x:xs) Nothing _ = encode' xs (Just x) 1
                  encode' (x:xs) (Just y) n
                    | x /= y = (fromCount y n):(encode' xs (Just x) 1)
                    | otherwise = encode' xs (Just x) (n + 1)
                  encode' [] (Just x) n = [(fromCount x n)]
                  encode' [] Nothing _ = []
                  fromCount :: a -> Int -> Variable a
                  fromCount a n | n == 1 = Single a
                                | otherwise = Multiple n a

-- aww yeah. that was tough.