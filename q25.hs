-- Problem 25

-- Generate a random permutation of the elements of a list.

import System.Random
import Data.List(nub)

-- from 23
randSelect' gen xs n = take (min (length xs) n) [xs !! n | n <- nub randomIndices]
                       where randomIndices = randomRs (0, (length xs) - 1) gen

permutation gen xs = randSelect' gen xs (length xs)

-- use standard generator
permutation' = permutation (mkStdGen 1)
