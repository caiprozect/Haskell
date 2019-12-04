import System.IO
import Data.List
import Data.Bits
import Data.List.Split
import Data.Char

decode :: [Int] -> [Int] -> [Int]
decode code pass = zipWith xor code (cycle pass)

check :: [Int] -> Bool
check plain = all (\x -> (32 <= x && x <= 122)) plain 

headCheck :: [Int] -> Bool
headCheck plain = "(The" == map chr xs
  where xs = take 4 plain

main :: IO ()
main = do
  withFile "cypher.txt" ReadMode $ \handle -> do
    text <- hGetContents handle
    let code = map read . splitOn "," $ text :: [Int]
    let result = head . filter headCheck . map (decode code) $ [[a,b,c] | a <- [97..122], b <- [97..122], c <- [97..122]] 
    print $ map chr result
    print $ sum result
