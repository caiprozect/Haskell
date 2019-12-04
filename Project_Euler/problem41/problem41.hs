import Data.List (sort)

isPrime :: Int -> Bool
isPrime x = all (/=0) . map (x `mod`) $ [2..floor . sqrt . fromIntegral $ x]

isPan :: Int -> Bool
isPan x = (sort . show $ x) == (take (length . show $ x) ['1'..])

maxPanPrime = head . filter (\x -> isPan x && isPrime x) $ [7654321, 7654320..1]

main :: IO ()
main = do
  print maxPanPrime
