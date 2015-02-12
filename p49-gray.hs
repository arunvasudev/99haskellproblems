-- Problem 49
-- graycode generation without caching
grayN :: Int -> [String]
grayN 0 = [""]
grayN n = let subList = grayN (n - 1) in
          (map ('0':) subList) ++ (map('1':) (reverse subList))
