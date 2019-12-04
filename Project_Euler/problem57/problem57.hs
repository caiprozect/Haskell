import Data.Ratio
import Data.List

expandAStep :: Rational -> Int -> Rational
expandAStep x _ = 1 + 1/(2+(x-1))

check :: Rational -> Bool
check x = (length . show . denominator $ x) < (length . show . numerator $ x)

main :: IO ()
main = do
  print $ length . filter check $ scanl' expandAStep (1 % 1) [1..1000]
