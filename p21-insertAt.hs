-- Problem 21 
-- Insert an element at a given position
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n - 1) xs)
