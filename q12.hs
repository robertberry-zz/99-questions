-- Problem 12

-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version.

data Variable a = Multiple Int a |
                  Single a
                  deriving (Show)

decodeModified :: Eq a => [(Variable a)] -> [a]

decodeModified xs = concatMap decode xs
                    where decode (Single x) = [x]
                          decode (Multiple n x) = replicate n x
