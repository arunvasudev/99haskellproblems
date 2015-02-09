import Data.List

lsort :: [[a]] -> [[a]]
lsort xs = sortBy (\xs' ys' -> compare (length xs') (length ys')) xs
