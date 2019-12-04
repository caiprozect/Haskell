import System.IO
import Data.List
import Data.List.Split

step :: [Int] -> [Int] -> [Int]
step ps ns = zipWith sumMax (zipWith (\a b -> [a, b]) (0:ps) (ps++[0])) ([head ns] : ((map (replicate 2) $ tail . init $ ns) ++ [[last ns]]))
  where sumMax p n = maximum $ zipWith (+) p n 

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    text <- hGetContents handle
    let lns = lines text
    let triangle = map (map read . splitOn " ") lns :: [[Int]]
    let finals = foldl1' step triangle
    let result = maximum finals
    print finals
    print result
