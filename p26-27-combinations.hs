import Data.List

--Problem 26
--Get all combinations of n items taken from a list
combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n xs = let ts = filter ((>= n) . length) . tails $ xs 
                        subs t' ts' = map (t':) [subCombs | subCombs <- combinations (n - 1) ts' ] in 
                    concat [subs (head ts'') (tail ts'') | ts'' <- ts]

-- Problem 27-b
-- Get all the possible groupings as per a given list of numbers
groups :: (Eq a) => [Int] -> [a] -> [[[a]]]
groups _ [] = [[]]
groups [] _ = [[]]
groups (n:ns) xs = let nCombs = combinations n xs in
                       concat [[comb:subGroups | subGroups <- groups ns (xs \\ comb)] | comb <- nCombs]
