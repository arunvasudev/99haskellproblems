import Data.List

data Tree a = Node a [Tree a] deriving (Eq, Show)

-- test data
tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []]
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 = Node 'a' [Node 'f' [Node 'g' []], Node 'c' [], Node 'b' [Node 'd' [], Node 'e' []]]

-- P70C count the number of nodes in a multiway tree
nnodes :: Tree a -> Int
nnodes (Node a xs) = 1 + sum [nnodes t | t <- xs]

-- P70 - converts a depth first representation of a string to tree
strToTreeLoop :: [Tree Char] -> String -> ([Tree Char], String)
strToTreeLoop soFar str =
    case str of 
        [] -> (reverse soFar, [])
        ('^':str') -> (reverse soFar, str')
        (c:str') -> let (chTs, str'') = strToTreeLoop [] str'
                        t = Node c chTs in
                        strToTreeLoop (t:soFar) str''

strToTree :: String -> Tree Char
strToTree str = let (ts, remStr) = strToTreeLoop [] str in
                    if (length ts /= 1) then error "Multiple nodes at the top level - invalid string"
                    else if (remStr /= "") then error "Couldn't completely munch the input - invalid string"
                    else head ts

-- P71 - determine the internal path length of a tree
ipl :: Tree a -> Int 
ipl (Node a xs) = sum [ipl' 0 t | t <- xs]

ipl' :: Int -> Tree a -> Int
ipl' plSoFar (Node a xs) = 
    let parentPL = 1 + plSoFar in
    parentPL + (sum [ipl' parentPL t | t <- xs])

-- P72 - create a bottom up sequence of the nodes of a multi-way tree
bottomUp :: Tree Char -> String
bottomUp (Node c xs) = (concat [bottomUp t | t <- xs]) ++ [c]

-- P73A - get the lispy string representation from a multiway tree
toLispyString :: Tree Char -> [Char]
toLispyString tree = 
    case tree of
        Node c [] -> [c]
        Node c xs -> '(':c:' ':(concat (intersperse " " [toLispyString t | t <- xs])) ++ ")"
