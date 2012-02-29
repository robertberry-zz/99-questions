-- Problem 20

-- Remove the K'th element from a list.

removeAt :: Int -> [a] -> (a, [a])

removeAt n xs
  | n < 0 = error "removeAt only accepts positive indices"
  | otherwise = removeAt' n xs []
                where removeAt' :: Int -> [a] -> [a] -> (a, [a])
                      removeAt' 0 (x:xs) acc = (x, (reverse acc) ++ xs)
                      removeAt' n (x:xs) acc = removeAt' (n - 1) xs (x:acc)
  
