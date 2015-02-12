-- Functions for boolean operators
and' = (&&)
or' = (||)
not' = not
nand a b = not (a && b)
nor a b = not (a || b)

xor :: Bool -> Bool -> Bool
xor a b = not (a == b)

impl :: Bool -> Bool -> Bool
impl a b = (not b) || a

equ :: Bool -> Bool -> Bool
equ = (==)

-- Problem 46 
-- Print the truth table for a given function
table2 :: (Bool -> Bool -> Bool) -> [String]
table2 f = [ (show a) ++ "  " ++ (show b) ++ "  " ++ (show (f a b)) | a <- [True, False], b <- [True, False]]

table2Print :: (Bool -> Bool -> Bool) -> IO ()
table2Print f = putStr . unlines $ (table2 f) 
