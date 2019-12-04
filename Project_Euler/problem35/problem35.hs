import Data.List

isPrime :: Int -> Bool
isPrime n = all (/=0) . map (n `mod`) $ [2..k]
  where k = floor . sqrt . fromIntegral $ n

primesUnderM :: [Int]
primesUnderM = filter isPrime [2..1000000]

rotaMin :: String -> String
rotaMin str = suffix ++ prefix
  where (prefix, suffix) = break (== minimum str) str

rotations :: String -> [Int]
rotations str = map (read . head) . group . sort $ zipWith (\f x -> f x) (scanl (.) rotate (replicate (length str - 1) rotate)) (repeat str)
  where rotate (x:xs) = xs ++ [x]

circularP = filter check primesUnderM
  where check n = all isPrime $ rotations $ show n

main :: IO ()
main = do
  let result = length circularP
  print result
