import Data.List
import Data.Ratio
import Data.Char

expApprox :: [Rational]
expApprox = 2 : (concat $ zipWith (\[x,y,z] k -> [x,y*k,z]) (repeat [1,2,1]) [1..])

convergent :: [Rational] -> Rational
convergent cf = foldr1 (\x y -> x + (denominator y % numerator y)) $ cf

intSqrt :: Int -> Double
intSqrt = sqrt . fromIntegral

isInt :: Double -> Bool
isInt num = num == (fromIntegral . floor $ num)

isSqrt :: Double -> Bool
isSqrt num = isInt . sqrt $ num

getInit :: Double -> (Double, (Double, Double), [(Double, Double)], [Double])
getInit num = (num, (1, d), [(1, d)], [d])
  where d = fromIntegral . floor . sqrt $ num

safelast :: [(Double, Double)] -> (Double, Double)
safelast [] = (0, 0)
safelast lst = last lst

process :: (Double, (Double, Double), [(Double, Double)], [Double]) -> [Double]
process (origin, (num, denom), soFar, cf) =
  let
    next = fromIntegral $ floor (num / (sqrt origin - denom))
    num' = (origin - (denom^2)) / num
    denom' = num' * next - denom
  in
    case (safelast . init $ soFar) == (num', denom')  of
      True -> reverse $ cf
      False -> process (origin, (num', denom'), (num', denom') : soFar, next : cf)

format :: Double -> Rational
format x = toRational x

findSolution :: Double -> (Integer, Double)
findSolution num = (x, num)
  where
    x = numerator . convergent . map format . checkInit . process . getInit $ num

checkInit :: [Double] -> [Double]
checkInit lst =
  case even . length $ lst of
    True -> init $ lst ++ tail lst
    False -> init lst

result :: Double
result = snd . last . sort . map findSolution . filter (not . isSqrt) $ [1..1000]

main :: IO ()
main = do 
  print result
