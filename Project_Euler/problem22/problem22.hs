import System.IO
import Data.List
import Data.List.Split

alphaToInt :: Char -> Int
alphaToInt c = length ['A'..c]

main :: IO ()
main = do
  withFile "names.txt" ReadMode $ \handle -> do
    text <- hGetContents handle
    let lns = map (init . tail) . splitOn "," $ text
    let names = sort lns
    let result = sum . map (\(a,b) -> a*b) $ zip (map (sum . map alphaToInt) $ names) [1..]
    print result
    
