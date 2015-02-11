import qualified Util as U

-- Problem 31
-- Determine whether a number is prime
isPrime :: Int -> Bool
isPrime n = let intSqrt = ceiling . sqrt . fromIntegral $ n in
                if (n == 2) then True else not . any (\n' -> n `mod` n' == 0) $ [2..intSqrt]

-- Problem 32
-- Get the GCD of two integers
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b = (gcd a b) == 1

-- Problem 34
-- calculate the Euler's totient function for n
totientPhi :: Int -> Int
totientPhi n = length . filter (coprime n) $ [1..n]

-- get all the prime numbers in a given range
primes :: Int -> Int -> [Int]
primes a b = filter isPrime [a..b]

-- if k is the maximum power of p that divides n, returns a list containing
-- p k times
sameDivisors :: Int -> Int -> Int -> [Int]
sameDivisors n p acc = if (n `mod` acc == 0) then p:(sameDivisors n p (acc * p)) else [] 

-- problem 35
-- determine the prime factors of a given number in ascending order
primeFactors :: Int -> [Int]
primeFactors n = concat [sameDivisors n p p | p <- primes 2 n]

-- problem 36
-- returns the prime factors along with their multiplicity
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = U.encode . primeFactors $ n

-- problem 37
-- Euler's totient function - improved
totientPhiImp :: Int -> Int
totientPhiImp n = product . map (\(p, k) -> (U.pow p (k - 1)) * (p - 1)) . primeFactorsMult $ n

-- problem 39
-- Given a range of numbers, construct a list of primes in that range
primesR = primes

-- problem 40
-- print a pair of primes that sum to a given number
goldbach :: Int -> (Int, Int)
goldbach n = let ps = primesR 2 n 
                 gbHelper [] n = error "Search failed"
                 gbHelper (p:ps') n = if (isPrime (n - p)) then (p, n - p) else gbHelper ps' n in
             gbHelper ps n

-- returns the list of all goldbach primes for a given number
goldbachList' :: Int -> [(Int, Int)]
goldbachList' n = let ps = primesR 2 (n `div` 2) 
                      gblHelper [] n = []
                      gblHelper (p:ps') n = if (isPrime (n - p)) 
                                            then (p, n - p):(gblHelper ps' n) 
                                            else gblHelper ps' n in
                  gblHelper ps n

goldbachList'' :: Int -> [(Int, (Int, Int))]
goldbachList'' n = [(e, pair) | e <- filter (\x -> x `mod` 2 == 0) [3..n], pair <- goldbachList' e]

goldbachListLim :: Int -> Int -> [String]
goldbachListLim n lim = [(show e) ++ " = " ++ (show p1) ++ " + " ++ (show p2)|(e, (p1, p2)) <- goldbachList'' n, p1  > lim && p2 > lim]

goldbachList1 :: Int -> Int -> [(Int, Int)]
goldbachList1 a b = map goldbach . filter ((== 0). (`mod` 2)) $ [a..b]

-- Problem 41
-- Returns the list of numbers whose Goldbach compositions' primes are above 50
goldbachListStr :: Int -> Int -> Int -> [String]
goldbachListStr a b lim = [show (p1 + p2) ++ " = " ++ (show p1) ++ " + " ++ (show p2) | (p1, p2) <- goldbachList1 a b, p1 > lim && p2 > lim]
