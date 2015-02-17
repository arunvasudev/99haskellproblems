data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Eq)

instance (Show a) => Show (Tree a) where
    show Empty = "."
    show (Branch v t1 t2) = "(" ++ (show v) ++ " " ++ (show t1) ++ " " ++ (show t2) ++ ")" 

-- problem 55, construct all the balanced trees
cBalancedTrees:: Int -> a -> [Tree a]
cBalancedTrees n a 
    | n <= 0 = [Empty]
    | (n-1) `mod` 2 == 0 = let subN = (n - 1) `div` 2
                               subTs = cBalancedTrees subN a in
                            [Branch a l r | l <- subTs, r <- subTs]
    | otherwise = let subN1 = (n - 1) `div` 2
                      subN2 = subN1 + 1
                      subTs1 = cBalancedTrees subN1 a 
                      subTs2 = cBalancedTrees subN2 a in
                   concat [[Branch a t1 t2, Branch a t2 t1] | t1 <- subTs1, t2 <- subTs2]

-- returns a left-right flipped over tree
flipTree :: Tree a -> Tree a
flipTree tree = case tree of
                    Empty -> Empty
                    (Branch v t1 t2) -> Branch v (flipTree t2) (flipTree t1)

-- returns true if the two trees have the same structure
areIsomorphic :: Tree a -> Tree a -> Bool
areIsomorphic t1 t2 = case (t1, t2) of
                        (Empty, Empty) -> True
                        (Branch v1 t1 t2, Branch v2 s1 s2)  -> areIsomorphic t1 s1 && areIsomorphic t2 s2
                        otherwise -> False

-- problem 56
-- returns true if the given tree is symmetric about a line through its root
-- node
isSymmetric :: Tree a -> Bool
isSymmetric t = let t' = flipTree t in
                areIsomorphic t t'

-- problem 57
-- adds a value to a binary search tree
add :: (Ord a) => Tree a -> a -> Tree a
add Empty a = Branch a Empty Empty
add node@(Branch v t1 t2) a 
    | a < v = Branch v (add t1 a) t2
    | a > v = Branch v t1 (add t2 a)
    | otherwise = node

-- problem 57 (contd)
-- creates a tree from a list of items
treeFromList :: (Ord a) => [a] -> Tree a
treeFromList xs = foldl (\accum v-> add accum v) Empty xs

-- problem 58
-- generate all completely balanced trees with a given number of nodes
symCBalancedTrees :: Int -> a -> [Tree a]
symCBalancedTrees n v = filter isSymmetric (cBalancedTrees n v)
