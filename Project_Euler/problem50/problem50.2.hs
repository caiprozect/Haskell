import Data.List

primes :: [Int]
primes = sieve [2..]
  where
    sieve (x:xs) = x : (sieve . filter (\n -> n `mod` x /= 0) $ xs)

maybePrime :: Int -> Maybe Int
maybePrime x =
  case all (/=0) . map (x `mod`) $ [2..(floor . sqrt . fromIntegral $ x)] of
    True -> Just x
    False -> Nothing

consec :: Maybe Int
consec = snd . maximum . filter (\x -> snd x /= Nothing) . concat . map (zip [1..]) . map (map maybePrime) . map (takeWhile (1000000>=)) . map (scanl1 (+)) . init . tails . takeWhile (1000000>=) $ primes

main :: IO ()
main = do
  print consec
