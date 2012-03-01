-- Problem 24

-- Draw N different random numbers from the set 1..M.

import System.Random
import Data.List(nub)

diffSelect gen n max = take (min n max) $ nub $ randomRs (1, max) gen

stdDiffSelect = diffSelect (mkStdGen 1)
