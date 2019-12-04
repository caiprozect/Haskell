import Data.Char
import System.IO
import Data.List.Split

isTriangleWord :: String -> Bool
isTriangleWord str = num == root * (root + 1)
  where num = 2 * (sum . map (\x -> ord x - 64) $ str) 
        root = floor . sqrt . fromIntegral $ num

main :: IO ()
main = do
  withFile "words.txt" ReadMode $ \handle -> do
    text <- hGetContents handle
    let ws = map (init . tail) . splitOn "," $ text
    let len = length . filter isTriangleWord $ ws
    print len
