-- Problem 27

-- Group the elements of a set into disjoint subsets.

--group :: [Int] -> [a] -> [[a]]

-- OK, let's subdivide the problem. First I want a function that given any list
-- gives all combinations of splits of the list into two, one of a given
-- length, the other containing all the remaining elements.

splits :: Int -> [a] -> [([a], [a])]

splits 1 xs = splits' xs []
              where splits' :: [a] -> [a] -> [([a], [a])]
                    splits' (x:xs) acc = ([x], acc ++ xs):(splits' xs (acc ++ [x]))
                    splits' [] _ = []

splits n xs = concatMap extendSplit (splits (n - 1) xs)
              where extendSplit (given, rest) =
                      [(given ++ x, rst) | (x, rst) <- splits 1 rest]

-- gah this does permutations not combinations

-- TO DO still