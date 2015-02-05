-- Problem 11
-- run length code a list
-- e.g., encode "aaabbcc" = [(3, 'a'), (2, 'b'), (2, 'c')]
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs@(y:ys) = (length run, y) : encode rem where
                        run = takeWhile (== y) xs
                        rem = dropWhile (== y) xs


data Encoded a = Multiple Int a | Single a
                 deriving (Show, Eq)

-- Problem 12
-- e.g., encodeModified "aaabbccd" = [Multiple 3 'a', Multiple 2 'b', Multiple 2 'c', Single 'd'] 
encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified xs = encodeModifiedImpl (encode xs) where
                        encodeModifiedImpl [] = []
                        encodeModifiedImpl ((n, e):xs) = (if (n == 1) 
                                                          then (Single e) 
                                                          else (Multiple n e)):(encodeModifiedImpl xs)


-- Problem 13
-- Decodes the output of problem 12 into the original array
decodeModified :: (Eq a) => [Encoded a] -> [a]
decodeModified xs = (concat . convertCodeToList) xs where
                        convertCodeToList [] = []
                        convertCodeToList (x:xs) = (case x of
                                                        Single a -> [a]
                                                        Multiple n a -> replicate n a):(convertCodeToList xs)

-- Problem 14
-- Encodes the array as in problem 12 but without the intermediate step of
-- calling function encode from problem 11
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect ys@(x:xs) = (let n = length (takeWhile (==x) ys) in
                          if (n == 1) then (Single x) else (Multiple n x)):(encodeDirect (dropWhile (==x) ys))
