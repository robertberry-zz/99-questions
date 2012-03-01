-- Problem 28

-- Sorting a list of lists according to length of sublists

-- a) We suppose that a list contains elements that are lists themselves. The
-- objective is to sort the elements of this list according to their
-- length. E.g. short lists first, longer lists later, or vice versa.

import Data.List
import Data.Ord
import Data.Function(on)

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

-- b) Again, we suppose that a list contains elements that are lists
-- themselves. But this time the objective is to sort the elements of this list
-- according to their length frequency; i.e., in the default, where sorting is
-- done ascendingly, lists with rare lengths are placed first, others with a
-- more frequent length come later.

lfsort :: [[a]] -> [[a]]

lfsort = concat . lsort . lgroup . lsort
         where lgroup = groupBy ((==) `on` length)
