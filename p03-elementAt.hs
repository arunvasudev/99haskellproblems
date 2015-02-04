-- get the k-th element of a list. The first element is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "No elements in an empty list"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)
