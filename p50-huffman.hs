-- problem 50
-- Given a list of (Symbol, Freq) pairs, generate the huffman coding for the
-- symbols
import Data.List

data Tree a = Leaf {value:: a , freq::Int} | 
              Branch {left::(Tree a), right::(Tree a), freq::Int} deriving (Show, Eq)

instance Eq a => Ord (Tree a) where
    compare t1 t2 = (freq t1) `compare` (freq t2)

huffman :: [(Char, Int)] -> [(Char, String)]
huffman syms = let initNodes = sort . map (\(c, f) -> (Leaf c f)) $ syms
                   combine nodes = case nodes of
                                     (x:[]) -> (x:[])
                                     (x:y:rem) -> let newNode = Branch x y ((freq x) + (freq y)) in
                                                      combine (insertSorted newNode rem) where
                                                            insertSorted x [] = [x]
                                                            insertSorted x (y:ys) = if (x < y) 
                                                                                    then (x:y:ys)
                                                                                    else (y:(insertSorted x ys))
                   encode tree = case tree of
                                    (Leaf c f) -> [(c, "")]
                                    (Branch l r f) -> let leftCodes = [(c, '0':code) | (c, code) <- encode l]
                                                          rightCodes = [(c, '1':code) | (c, code) <- encode r] in
                                                       leftCodes ++ rightCodes
                   codeComparer (c1, code1) (c2, code2) = (length code1, code1) `compare` (length code2, code2)
                   in sortBy codeComparer . encode . head . combine $ initNodes  
