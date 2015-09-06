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
toLispy :: Tree Char -> [Char]
toLispy tree = 
    case tree of
        Node c [] -> [c]
        Node c xs -> '(':c:' ':(concat (intersperse " " [toLispy t | t <- xs])) ++ ")"

-- P73B - convert a lispy string to a multiway tree
fromLispy' :: [Char] -> (Maybe (Tree Char), [Char])
fromLispy' [] = (Nothing, [])
fromLispy' (c:cs) = 
    case c of
        '(' -> case cs of
                [] -> error "Expected tokens after '(', but found none"
                (c':cs') -> 
                    case c' of
                        ')' -> error "Expected character after '(', but found ')'"
                        _ -> let (children, cs'') = fromLispyLoop cs'
                             in (Just (Node c' children), cs'')
        ')' -> (Nothing, cs)
        _ -> (Just (Node c []), cs)

fromLispyLoop :: [Char] -> ([Tree Char], [Char])
fromLispyLoop cs = 
    let (nextTree, cs') = fromLispy' cs in
       case nextTree of
           Nothing -> ([], cs')
           Just t -> 
                let (ts, cs'') = fromLispyLoop cs' in
                (t:ts, cs'')

fromLispy ::[Char] -> Tree Char
fromLispy cs =
    let cs' = filter (\x -> x /= ' ') cs
        (m, cs'') = fromLispy' cs' in
    case cs'' of
        [] -> case m of
                Nothing -> error "Parse failure - couldn't get a tree"
                Just t -> t
        _ -> error "Failed to parse the input lispy string"
