largestPF :: Int -> Int
largestPF num = last primePF
  where primePF = filter f [2..(floor . sqrt . fromIntegral $ num)]
        f x = num `mod` x == 0 && isPrime x

isPrime :: Int -> Bool
isPrime x = not . any (==0) $ map (x `mod`) [2..(floor . sqrt . fromIntegral $ x)]

main :: IO ()
main = do
  let num = 600851475143
  let result = largestPF num
  print result
