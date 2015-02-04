-- need to create this type because Lists are homogeneous in Haskell
data NestedList a = Elem a | List [NestedList a]

-- flattens a nested list
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
