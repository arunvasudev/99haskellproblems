-- Problem 18
-- Extract a slice from a list, the range of the indices of the slice being
-- given
slice :: [a] -> Int -> Int -> [a]
slice xs start end = (drop (start - 1) . take end) xs
