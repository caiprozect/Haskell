decimalToBinary :: Int -> [Int]
decimalToBinary 0 = [0]
decimalToBinary 1 = [1]
decimalToBinary n = decimalToBinary (n `div` 2) ++ [n `mod` 2]

doubleBasePalin :: [Int]
doubleBasePalin = filter check [1..1000000]
  where check x = reverse (show x) == show x && reverse (decimalToBinary x) == decimalToBinary x

main :: IO ()
main = do
  let result = sum doubleBasePalin
  print result
