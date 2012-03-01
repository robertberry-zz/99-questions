-- Problem 7
--
-- Flatten a nested list structure

-- this mess is for flattening and containing the original type structure. I
-- then realized the initial problem actually asked for the return type to be a
-- normal in built list rather than this crazy type. So flatten'.
data List a = List [List a] |
              Elem a
              deriving (Show)

flatten :: List a -> List a
flatten (List ((Elem x):xs)) = List ((Elem x):rest)
                               where (List rest) = (flatten (List xs))
flatten (List (x:xs)) = List (inner ++ rest)
                        where (List inner) = (flatten x)
                              (List rest) = (flatten (List xs))
flatten x = x


-- proper solution

flatten' :: List a -> [a]
flatten' (List []) = []
flatten' (Elem x) = [x]
flatten' (List (x:xs)) = (flatten' x) ++ (flatten' (List xs))

-- fuck's sake that was so much easier D: