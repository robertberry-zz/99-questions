-- Problem 9

-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements, they should be placed in a separate sublists.

pack :: Eq a => [a] -> [[a]]

pack [] = []
pack (x:xs) = (x:siblings):(pack rest)
              where (siblings, rest) = break (\y -> y /= x) xs
