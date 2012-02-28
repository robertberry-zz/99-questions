-- Problem 1
--
-- Find the last element of a list

myLast [] = error "empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs
