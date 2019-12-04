import Data.List

type Info = [Int]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = all (/=0) . map (x `mod`) $ [2..floor . sqrt . fromIntegral $ x]

foldInfo :: Info -> Int -> Info
foldInfo [pL, wL, sideLen] _ = [lenPrime, wL+4, sideLen+2]
  where
    preLast = 1 + 4 * (sum . filter even $ [2..sideLen-1])
    lenPrime = pL + (length . filter isPrime . tail $ scanl' (+) preLast (replicate 4 (sideLen+1))) 

check :: Info -> Bool
check [pL, wL, sideLen] = 0.1 > (fromIntegral pL :: Double) / (fromIntegral wL :: Double)

main :: IO ()
main = do
  print $ last . head . filter check . tail $ scanl' foldInfo [0, 1, 1] [1..]  
