import Data.List
import qualified Data.Map as M

-- Problem 28-a
-- Sort a list of lists by the length of their elements
lsort :: [[a]] -> [[a]]
lsort xs = sortBy (\xs' ys' -> compare (length xs') (length ys')) xs

-- Problem 28-b
-- Sort a list of lists by the frequency of the lengths of their elements
lsortf :: [[a]] -> [[a]]
lsortf xs = let addLenFreqToMap len map = if (M.member len map) then (M.insert len ((map M.! len) + 1) map) 
                                          else (M.insert len 0 map)
                freqMap = foldr addLenFreqToMap M.empty (map length xs)
                sorter fm xs' ys' = compare ((fm M.! length xs'), length xs') ((fm M.! length ys'), length ys') in
            sortBy (sorter freqMap) xs

-- Problem 28-b
-- Second version using pack, not much different from first version.
pack :: (Eq a) => [a] -> [(a,Int)]
pack [] = []
pack ys@(x:xs) = (x, length $ takeWhile (== x) ys):(pack (dropWhile (== x) ys))

lsortf2 :: [[a]] -> [[a]]
lsortf2 xs = let lenFreqs = pack . sort $ map length xs 
                 freqMap = M.fromList lenFreqs
                 sorter fm xs' ys' = compare ((fm M.! length xs'), length xs') ((fm M.! length ys'), length ys') in
             sortBy (sorter freqMap) xs

