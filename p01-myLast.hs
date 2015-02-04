myLast :: [a] -> a
myLast [] = error "No last element for an empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

