-- Problem 21 
-- Insert an element at a given position
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = if (n <= 0) 
                  then x:xs
                  else if (n > (length xs)) 
                       then xs ++ [x]
                       else (take (n-1) xs) ++ [x] ++ (drop (n - 1) xs)
