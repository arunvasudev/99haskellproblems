-- Problem 17
-- split a list into two parts, the length of the first being given
split xs n = (take n xs, drop n xs)
