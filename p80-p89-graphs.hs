import Data.List as L
import Data.List.Split as S
import Data.Map as M

-- represents a single node of the graph
-- and its adjacent edges
data Node a = Node a [a] deriving (Eq, Show)

-- P80A - convert a human friendly string form of
-- a graph to its equivalent graph term form
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

toGraphTerm strs 
    = let strs' = S.splitOn "," (L.filter (/= ' ') strs)
          nodes = M.empty in
          toGraphTerm' strs' nodes
