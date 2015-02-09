-- Problem 31
-- Determine whether a number is prime
isPrime :: Int -> Bool
isPrime n = let intSqrt = ceiling . sqrt . fromIntegral $ n in
                if (n == 2) then True else not . any (\n' -> n `mod` n' == 0) $ [2..intSqrt]
