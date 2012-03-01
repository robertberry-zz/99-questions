-- Problem 23

-- Extract a given number of randomly selected elements from a list.

import System.Random
import Data.List(nub)

-- this one includes duplicate elements
  
randSelect gen xs n = take n [xs !! n | n <- randomIndices]
                      where randomIndices = randomRs (0, (length xs) - 1) gen

stdRandSelect = randSelect (mkStdGen 1)

-- this one does not

randSelect' gen xs n = take (min (length xs) n) [xs !! n | n <- nub randomIndices]
                       where randomIndices = randomRs (0, (length xs) - 1) gen

stdRandSelect' = randSelect' (mkStdGen 1)
