-- Problem 8

-- Eliminate consecutive duplicates of list elements

compress :: Eq a => [a] -> [a]

compress [] = []
compress (x1:(x2:xs)) | x1 == x2 = compress (x2:xs)
                      | otherwise = x1:(compress (x2:xs))
compress (x:[]) = [x]
