-- packs consecutive duplicate elements of a list into sublists
-- E.g., pack "aaabbcc" == ["aaa", "bb", "cc"]
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs@(y:ys) = (takeWhile (== y) xs) : pack (dropWhile (== y) xs)
