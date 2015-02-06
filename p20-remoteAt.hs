-- Problem 20
-- Remove the k'th element of a list
removeAt :: [a] -> Int -> Maybe ([a], a)
removeAt xs n = if (n <= 0 || n > (length xs)) 
                then Nothing
                else (Just ((take (n - 1) xs) ++ (drop n xs), xs !! (n -1)))
