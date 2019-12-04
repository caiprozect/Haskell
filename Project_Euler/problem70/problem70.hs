import Data.Numbers.Primes
import Data.List

eulerPi :: Double -> Double
eulerPi num = num * (product . map (\x -> 1 - 1/(fromIntegral x)) $ factors)
  where factors = nub . primeFactors . floor $ num

measure :: Double -> Double
measure num = num / eulerPi num

totientPermut :: Double -> (Double, Double)
totientPermut num = 
	let
	  p = eulerPi num
	in
	case (sort . show . floor $ num) == (sort . show . round $ p) of
		True -> (num / p, num)
		False -> (0,0)

main :: IO ()
main = do
	print $ snd . minimum . filter (/=(0,0)) . map totientPermut $ [2..10^7]