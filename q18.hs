-- Problem 18

-- Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits
-- included). Start counting the elements with 1.

slice :: [a] -> Int -> Int -> [a]

slice _ i k | i < 0 = error "must supply positive indices for slice"
            | k < i = error "second index must be higher than first for slice"

slice (x:xs) i k | i > 0 = slice xs (i - 1) k
                 | k > 0 = x:(slice xs i (k - 1))
                 | otherwise = []

slice [] _ _ = []
