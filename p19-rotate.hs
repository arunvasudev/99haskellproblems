-- Problem 19
-- Rotate a list n places to the left if n > 0, to the right if n < 0
rotate :: [a] -> Int -> [a]
rotate xs n = let len = length xs 
                  n' = (abs n) `mod` len 
                  n'' = len - n' in
                if (n > 0) 
                then (drop n' xs) ++ (take n' xs)
                else (drop n'' xs) ++ (take n'' xs)
