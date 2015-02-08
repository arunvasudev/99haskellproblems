import System.Random

removeAt :: Int -> [a] -> [a]
removeAt n xs = (take (n - 1) xs) ++ (drop n xs)

-- Problem 23
-- Extract a given number of random elements from a list
rndSelect :: (RandomGen g) => g -> Int -> [a] -> [a]
rndSelect _ 0 _ = []
rndSelect g n xs = if (n > (length xs)) then error "Input doesn't have enough elements"
                   else let (indx, g') = randomR (1, length xs) g
                            xs' = removeAt indx xs in
                        (xs!!(indx - 1)):(rndSelect g' (n - 1) xs')


-- Problem 24
-- Draw n different random numbers from within a given range
diffSelect :: (RandomGen g) => g -> Int -> Int -> Int -> [Int]
diffSelect g n a b = let xs = [a..b] in
                     rndSelect g n xs

-- Problem 25
-- Generate a random permutation of the elements of a list
rndPermu :: (RandomGen g) => g -> [a] -> [a]
rndPermu g xs = let len = length xs
                    indices = rndSelect g len [0..(len - 1)] in
                map (\i -> xs!!i) indices
                        
