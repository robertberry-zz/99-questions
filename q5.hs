-- Problem 5
--
-- Reverse a list

myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
