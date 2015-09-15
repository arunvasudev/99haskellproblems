import Data.List as L
import Data.List.Split as S
import Data.Map as M

-- represents a single node of the graph
-- and its adjacent edges
data Node a = Node a [a] deriving (Eq, Show)

g1 = "b-c, f-c, g-h, f-b, k-f, h-g"

-- P80A - convert a human friendly string form of
-- a graph to its equivalent graph term form
toGraphTerm' :: [[Char]] -> (M.Map Char [Char]) -> M.Map Char [Char]
toGraphTerm' strs nodes
    = case strs of
        [] -> nodes
        (str:strs') -> 
            case str of
                (x:[]) ->
                    let nodes' = M.insert x [] nodes in
                    toGraphTerm' strs' nodes'
                (x:'-':y:[]) -> 
                    let ls = M.findWithDefault [] x nodes
                        nodes' = M.insert x (y:ls) nodes in
                        toGraphTerm' strs' nodes'
                _ -> error "Invalid component in human friendly string"

toGraphTerm :: String -> M.Map Char [Char]
toGraphTerm strs 
    = let strs' = S.splitOn "," (L.filter (/= ' ') strs)
          nodes = M.empty in
          toGraphTerm' strs' nodes

-- P80B - convert a list of graph term terms to human friendly
-- form.
toHumanFriendly' terms =
    case terms of
      (x, []):terms'  -> [(x:"")] ++ (toHumanFriendly' terms')
      (x, ls):terms'  -> let expand x ls = 
                                case ls of
                                  (l:ls') -> (x:'-':l:""):(expand x ls')
                                  [] -> [] 
                         in (expand x ls) ++ (toHumanFriendly' terms')
      [] -> []

toHumanFriendly terms =
    let comps = toHumanFriendly' terms in
        L.intercalate "," comps

-- P80C 
-- Converts a list of nodes and edges into an adjacency list form
-- e.g., toGraph ['a','b','c'] [('a','b'), ('a', 'c'), ('c', 'a')]
toGraph :: (Ord a) => [a] -> [(a, a)] -> M.Map a [a]
toGraph nodes edges =
    let m = L.foldl (\acc x -> M.insert x [] acc) M.empty nodes
        m1 = L.foldl (\acc (s, e) -> 
                        let ls = M.findWithDefault [] s acc in
                            M.insert s (e:ls) acc) m edges
    in m1

-- P81C
-- Returns all the acyclic paths from nodes start to end in a graph
paths' :: (Ord a) => a -> a -> [a] -> M.Map a [a] -> [[a]]
paths' start end ps graph =
    if start == end then [L.reverse (end:ps)]
    else
        let ns' = M.findWithDefault [] start graph
            ns'' = L.filter (\x -> not (any (== x) ps)) ns'
            subs = L.foldl (\acc x -> 
                               let ps' = paths' x end (start:ps) graph 
                               in (ps' ++ acc)) [] ns''
        in subs 

paths :: (Ord a) => a -> a -> M.Map a [a] -> [[a]]
paths start end graph = paths' start end [] graph
