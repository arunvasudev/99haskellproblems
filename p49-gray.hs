import qualified Data.Map as M

-- Problem 49
-- graycode generation without caching
grayN :: Int -> [String]
grayN 0 = [""]
grayN n = let subList = grayN (n - 1) in
          (map ('0':) subList) ++ (map('1':) (reverse subList))
          

-- Problem 49-b
-- graycode with caching. Probably should use the State monad here.
type GrayCache = M.Map Int [String]
emptyCache :: GrayCache
emptyCache = M.empty

grayNCached :: GrayCache -> Int -> ([String], GrayCache)
grayNCached cache 0 = ([""], cache)
grayNCached cache n = if (M.member n cache) 
                      then (cache M.! n, cache)
                      else let (subList, cache') = grayNCached cache (n - 1)
                               list' = (map ('0':) subList) ++ (map('1':) (reverse subList))
                               cache'' = M.insert n list' cache' in
                               (list', cache'')


