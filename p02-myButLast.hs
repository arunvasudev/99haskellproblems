-- Get the last but one element of a list
myButLast :: [a] -> a
myButLast [] = error "No penultimate element for an empty list"
myButLast (x:[]) = error "No penultimate element for a singleton list"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs
