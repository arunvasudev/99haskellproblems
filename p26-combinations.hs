--Problem 26
--Get all combinations of n items taken from a list
import Data.List

combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n xs = let ts = filter ((>= n) . length) . tails $ xs 
                        subs t' ts' = map (t':) [subCombs | subCombs <- combinations (n - 1) ts' ] in 
                    concat [subs (head ts'') (tail ts'') | ts'' <- ts]
