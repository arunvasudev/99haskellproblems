-- Problem 14
-- Duplicates every element in a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)
