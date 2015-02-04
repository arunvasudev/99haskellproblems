-- reverses a list
myReverse xs = myReverseImpl [] xs where
                    myReverseImpl xs [] = xs
                    myReverseImpl xs (y:ys) = myReverseImpl (y:xs) ys

