-- Problem 26

-- Generate the combinations of K distinct objects chosen from the N elements
-- of a list

-- In how many ways can a committee of 3 be chosen from a group of 12 people?
-- We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
-- well-known binomial coefficients). For pure mathematicians, this result may
-- be great. But we want to really generate all the possibilities in a list.

combinations :: Int -> [a] -> [[a]]

combinations n _ | n < 0 = error "Must supply postive integer."
combinations 1 xs = [[x] | x <- xs]
combinations n (x:xs) = [x:rest | rest <- (combinations (n - 1) xs)] ++
                        combinations n xs
combinations _ [] = []
