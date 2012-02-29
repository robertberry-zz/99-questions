-- Problem 16

-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]

dropEvery xs n
  | n <= 0 = error "dropEvery n must be positive"
  | otherwise = dropEvery' xs n 1
                where dropEvery' :: [a] -> Int -> Int -> [a]
                      dropEvery' (x:xs) n acc
                        | acc < n = x:(dropEvery' xs n (acc + 1))
                        | otherwise = dropEvery' xs n 1
                      dropEvery' [] _ _ = []

