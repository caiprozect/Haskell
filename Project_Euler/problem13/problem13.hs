import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    text <- hGetContents handle
    let lns = map read . lines $ text :: [Integer]
    print $ take 10 . show . sum $ lns
    
