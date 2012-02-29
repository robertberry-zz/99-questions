-- Problem 19

-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).

rotate :: [a] -> Int -> [a]

rotate xs n | n < 0 = rotate' xs ((length xs) + n) []
            | otherwise = rotate' xs n []
  where rotate' :: [a] -> Int -> [a] -> [a]
        rotate' xs 0 acc = xs ++ (reverse acc)
        rotate' (x:xs) n acc = rotate' xs (n - 1) (x:acc)
