stPrime :: Int
stPrime = last . take 10001 . filter isPrime $ [2..]
  where isPrime x = all (/=0) . map (x `mod`) $ [2..(floor . sqrt . fromIntegral $ x)]

main :: IO ()
main = do
  print stPrime
