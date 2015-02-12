import Util (justifyShow)

-- Functions for boolean operators
and' = (&&)
or' = (||)
not' = not
nand a b = not (a && b)
nor a b = not (a || b)

xor' :: Bool -> Bool -> Bool
xor' a b = not (a == b)

impl' :: Bool -> Bool -> Bool
impl' a b = (not b) || a

equ' :: Bool -> Bool -> Bool
equ' = (==)

-- Problem 46
-- Print the truth table for a given function
table2 :: (Bool -> Bool -> Bool) -> [String]
table2 f = [ (show a) ++ "  " ++ (show b) ++ "  " ++ (show (f a b)) | a <- [True, False], b <- [True, False]]

table2Print :: (Bool -> Bool -> Bool) -> IO ()
table2Print f = putStr . unlines $ (table2 f) 

-- Problem 47 doesn't need any modification from problem 46

-- Generates all lists of boolean values of length n
boolN :: Int -> [[Bool]]
boolN 0 = [[]]
boolN n = let subList = boolN (n - 1) in
          (map (True:) subList) ++ (map (False:) subList)

tablen :: Int -> ([Bool] -> Bool) -> [String]
tablen n f = let rowFn bools = concat . map ((++ "  ") . (justifyShow 6)) $ bools in
             [(rowFn bs) ++ (justifyShow 6 (f bs)) | bs <- boolN n]

tablenPrint :: Int -> ([Bool] -> Bool) -> IO ()
tablenPrint n f = putStr . unlines $ (tablen n f)
