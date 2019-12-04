import Data.List
import Control.Parallel.Strategies

primes :: [Int]
primes = sieve [2..1000000]
  where
    sieve [] = []
    sieve (x:xs) = x : (sieve . filter (\n -> n `mod` x /= 0) $ xs)

consecLen :: Int -> Maybe Int
consecLen p = head . filter (/=Nothing) . map check . map (takeWhile (p>=)) . map (scanl1 (+)) . init . tails . takeWhile (p>=) $ primes
  where
    check lst =
      case last lst == p of
        True -> Just (length lst)
        False -> Nothing

consecPrimeSum :: Int
consecPrimeSum = snd . maximum . map (\p -> (consecLen p, p)) $ primes

consecLenM :: Int -> IO (Maybe Int, Int)
consecLenM p = do
  let maxLen = consecLen p
  print (p, maxLen)
  return (maxLen, p)

main :: IO ()
main = do
  lst <- mapM consecLenM primes
  let result = maximum lst
  print $ snd result

