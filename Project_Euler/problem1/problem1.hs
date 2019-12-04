import Data.List

multiplesOf3and5 :: Int -> Int
multiplesOf3and5 n = foldl' f 0 [1..(n-1)]
  where f acc x =
          case x `mod` 3 == 0 || x `mod` 5 == 0 of
            True -> acc + x
            False -> acc

main :: IO ()
main = do
  num <- getLine
  let n = read num
  let result = multiplesOf3and5 n
  print result
