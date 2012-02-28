-- Problem 4
--
-- Find the number of elements in a list.

myLength [] = 0
myLength (x:xs) = 1 + myLength xs
