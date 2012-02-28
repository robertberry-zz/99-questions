-- Problem 2
--
-- Find the last but one element of a list

lastButOne (x:_:[]) = x
lastButOne xs@(_:_:_) = lastButOne (tail xs)
lastButOne _ = error "List is too short."
