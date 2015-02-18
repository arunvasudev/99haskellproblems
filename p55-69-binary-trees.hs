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

-- problem 59
-- generate all the height balanced trees of a given height
hBalancedTrees :: Int -> a -> [Tree a]
hBalancedTrees 0 v = [Empty]
hBalancedTrees 1 v = [Branch v Empty Empty]
hBalancedTrees h v = let subTrees' = hBalancedTrees (h - 1) v
                         subTrees'' = hBalancedTrees (h - 2) v
                         nextGenUneql = concat [[Branch v t1 t2, Branch v t2 t1] | 
                                                    t1 <- subTrees', t2 <- subTrees'']
                         nextGenEql = [Branch v t1 t2 | t1 <- subTrees', t2 <- subTrees'] in
                     nextGenEql ++ nextGenUneql
                     
-- problem 60
-- Construct height balanced binary trees with a given number of nodes

-- Given a height h, what is the minimum number of nodes it can have if it's to
-- be height balanced? N(h) = 1 + N(h - 1) + N(h - 2)
minHBalancedNodeCount :: Int -> Int
minHBalancedNodeCount h = fst $ foldl (\(a, b) _ -> (b, 1 + a + b)) (0, 1) [1..h]

-- generates a series of (height, minHBalancedNodecounts for height)
minHBalancedNodeCounts :: [(Int, Int)]
minHBalancedNodeCounts = let helper h a b = (h, a):(helper (h + 1) b (1 + a + b))
                         in helper 0 0 1

-- given a number of nodes N, what's the maximum height a height balanced tree
-- can have?
maxHBalancedHeight :: Int -> Int
maxHBalancedHeight n = let (h, n') = head .dropWhile (\(h, n') -> n' < n) $ minHBalancedNodeCounts in
                       if (n' == n) then h else (h - 1)

minHBalancedHeight :: Int -> Int
minHBalancedHeight n 
        | n <= 0 = 0
        | otherwise = floor (log(fromIntegral $ n + 1)/log(2.0))

-- returns the number of nodes in a given tree
nodeCount :: Tree a -> Int
nodeCount Empty = 0
nodeCount (Branch v t1 t2) = 1 + (nodeCount t1) + (nodeCount t2)

-- returns all the height balanced trees with the given number of nodes
-- very inefficient implementation!
hBalancedTreeNodes :: Int -> a -> [Tree a]
hBalancedTreeNodes n v = let minHeight = minHBalancedHeight n
                             maxHeight = maxHBalancedHeight n in
                         [t | h <- [minHeight..maxHeight], t <- filter ((== n) . nodeCount) $ hBalancedTrees h v] 

-- problem 61
-- returns the number of leaves in the tree
leafCount :: Tree a -> Int
leafCount Empty = 0
leafCount (Branch v Empty Empty) = 1
leafCount (Branch v left right) = (leafCount left) + (leafCount right)

-- problem 61A
-- collect the leaves of a tree into a list
leavesToList :: Tree a -> [a]
leavesToList Empty = []
leavesToList (Branch v Empty Empty) = [v]
leavesToList (Branch v left right) = (leavesToList left) ++ (leavesToList right)

-- problem 62
-- collect the internal nodes of a tree to a list
internalsToList :: Tree a -> [a]
internalsToList Empty = []
internalsToList (Branch v Empty Empty) = []
internalsToList (Branch v left right) = (internalsToList left) ++ [v] ++ (internalsToList right)

-- problem 62b
-- collect the nodes of a binary tree at a given level into a list
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch v _ _) 1 = [v]
atLevel (Branch _ left right) n = (atLevel left (n-1)) ++ (atLevel right (n - 1))
