-- Problem 17

-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.

split :: [a] -> Int -> ([a], [a])

split xs n | n < 0 = error "split n must be positive"
           | otherwise = split' xs n []
                         where split' :: [a] -> Int -> [a] -> ([a], [a])
                               split' xs 0 acc = ((reverse acc), xs)
                               split' (x:xs) n acc = split' xs (n - 1) (x:acc)
                               split' [] n acc = ((reverse acc, []))
