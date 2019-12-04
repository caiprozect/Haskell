isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all (/=0) . map (n `mod`) $ [2..floor . sqrt . fromIntegral $ n]

truncatablePrimes :: [Int]
truncatablePrimes = take 11 . filter check $ [11..]
  where check x = all isPrime $ [x] ++ lts x ++ rts x
        lts x = zipWith mod (repeat x) (map (10^) $ take ((length . show $ x) - 1) [1..])
        rts x = zipWith div (repeat x) (map (10^) $ take ((length . show $ x) - 1) [1..])

main :: IO ()
main = do
  print $ sum truncatablePrimes
