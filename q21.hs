-- Problem 21

-- Insert an element at a given position into a list. 

insertAt :: a -> [a] -> Int -> [a]

insertAt y xs 0 = y:xs
insertAt y (x:xs) n | n < 0 = error "can only insert at positive index"
                    | otherwise = x:(insertAt y xs (n - 1))
insertAt y [] n = error "list is too short"
