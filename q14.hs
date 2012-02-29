-- Problem 14

-- Duplicate the elements of a list.

duplicate :: Eq a => [a] -> [a]

duplicate = concatMap (replicate 2)
