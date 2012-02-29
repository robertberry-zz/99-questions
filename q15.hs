-- Problem 15

-- Replicate the elements of a list a given number of times.

duplicate' :: Eq a => Int -> [a] -> [a]

duplicate' n = concatMap (replicate n)
