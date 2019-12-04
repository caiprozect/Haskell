import Data.List
import Data.Ratio
import Data.Char

expApprox :: [Rational]
expApprox = 2 : (concat $ zipWith (\[x,y,z] k -> [x,y*k,z]) (repeat [1,2,1]) [1..])

convergent :: Rational
convergent = foldr1 (\x y -> x + (denominator y % numerator y)) . take 100 $ expApprox

main :: IO ()
main = do
  print convergent
  print $ sum . map digitToInt . show . numerator $ convergent
