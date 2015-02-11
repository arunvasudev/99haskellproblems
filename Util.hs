module Util (
        pack
    ,   encode
    ,   pow
    ) where

-- packs consecutive duplicate elements of a list into sublists
-- E.g., pack "aaabbcc" == ["aaa", "bb", "cc"]
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs@(y:ys) = (takeWhile (== y) xs) : pack (dropWhile (== y) xs)

-- encodes a list with consecutive equal elements into a list with tuples of
-- the form (elem, elemCount)
encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode xs@(y:ys) = (y, length (takeWhile (== y) xs)):encode (dropWhile (== y) xs)

-- raises n to k
pow :: Int -> Int -> Int
pow 0 _ = 0
pow n 0 = 1
pow n k = n*(pow n (k - 1))
