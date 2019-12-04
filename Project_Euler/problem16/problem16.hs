import Data.Char
import Data.List

num = 2^1000 :: Integer

digitSum :: Int -> Char -> Int
digitSum acc d = acc + digitToInt d

main :: IO ()
main = do
  let ds = show num
  let result = foldl' digitSum 0 ds
  print result
