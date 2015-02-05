-- Problem 16
-- Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = let initial = if (length xs < n) then xs else (init . take n) xs 
                     remaining = drop n xs in
                  initial ++ (dropEvery remaining n)
