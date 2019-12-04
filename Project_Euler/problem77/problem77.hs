import Data.List
import Data.Numbers.Primes

genInit :: Int -> (Int, Int)
genInit num = (num, num-1)

part :: (Int, Int) -> [(Int, Int)]
part (num, p) = do
  a <- takeWhile (<=p) primes
  case num < 0 of
    True -> []
    False ->
      case num - a == 0 of
        True -> [(num-a, a)]
        False -> part (num-a, a)

main :: IO ()
main = do
  let x = length . takeWhile (< 5000) . map (length . part . genInit) $ [1..]
  print $ x + 1
  print $ length . part . genInit $ x + 1
