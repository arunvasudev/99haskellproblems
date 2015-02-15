data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq)

instance (Show a) => Show (Tree a) where
    show Empty = "."
    show (Branch v t1 t2) = "(" ++ (show v) ++ " " ++ (show t1) ++ " " ++ (show t2) ++ ")" 

-- problem 55, construct all the balanced trees
cBalancedTree:: Int -> a -> [Tree a]
cBalancedTree n a 
    | n <= 0 = [Empty]
    | (n-1) `mod` 2 == 0 = let subN = (n - 1) `div` 2
                               subTs = cBalancedTree subN a in
                            [Branch a l r | l <- subTs, r <- subTs]
    | otherwise = let subN1 = (n - 1) `div` 2
                      subN2 = subN1 + 1
                      subTs1 = cBalancedTree subN1 a 
                      subTs2 = cBalancedTree subN2 a in
                   concat [[Branch a t1 t2, Branch a t2 t1] | t1 <- subTs1, t2 <- subTs2]
