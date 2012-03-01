-- Problem 3
--
-- Find the K'th element of a list. The first element in the list is number 1.

elementAt (x:xs) 1 = x
elementAt (x:xs) n | n > 1 = elementAt xs (n - 1)
                   | otherwise = error "Cannot specify an index lower than 1."
elementAt [] n = error "List too short."
