-- run length code a list
-- e.g., encode "aaabbcc" = [(3, 'a'), (2, 'b'), (2, 'c')]
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs@(y:ys) = (length run, y) : encode rem where
                        run = takeWhile (== y) xs
                        rem = dropWhile (== y) xs
