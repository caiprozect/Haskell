import Data.List

primes :: [Int]
primes = sieve [2..]
  where
    sieve (x:xs) = x : filter (\n -> n `mod` x /= 0) xs

isPrime :: Int -> Bool
isPrime x = all (/=0) . map (x `mod`) $ [2..(floor . sqrt . fromIntegral $ x)]

primePermutation = take 2 . filter (/="") . map f . filter (> 999) $ primes
  where pLst p = map read . dropWhile (/=(show p)) . nub . sort . filter (isPrime . read) . permutations . show $ p :: [Int]
        f p = tryGenConsec $ pLst p
        tryGenConsec :: [Int] -> String
        tryGenConsec lst =
          case length lst < 3 of
            True -> ""
            False ->
              case elem (2*(head . tail $ lst) - head lst) (tail lst) of
                True -> concat . map show $ [head lst, head . tail $ lst, 2*(head . tail $ lst) - head lst]
                False -> tryGenConsec (head lst : (tail . tail $ lst))

main :: IO ()
main = do
  print primePermutation
