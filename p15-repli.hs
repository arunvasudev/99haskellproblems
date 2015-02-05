-- Problem 15
-- repli ['a', 'b', 'c'] 3 = "aaabbbccc"
repli:: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)
