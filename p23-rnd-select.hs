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
                        (xs !! indx):(rndSelect g' (n - 1) xs')

                        
