import Data.List

sieve :: [Int] -> [Int]
sieve ns =
  case head ns > (floor . sqrt . fromIntegral $ last ns) of
    True -> ns
    False -> (head ns) : sieve (filter (f (head ns)) ns)
             where f x a = 0 /= a `mod` x 

sumPrimes :: Int
sumPrimes = sum $ sieve [2..2000000]

main :: IO ()
main = do
  print sumPrimes
