import Data.Numbers.Primes
import Data.List

eulerPi :: Double -> Double
eulerPi num = num * (product . map (\x -> 1 - 1/(fromIntegral x)) $ factors)
  where factors = nub . primeFactors . floor $ num

measure :: Double -> Double
measure num = num / eulerPi num

main :: IO ()
main = do
  print $ snd . maximum . map (\x -> (measure x, x)) $ [1..1000000]