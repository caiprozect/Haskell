import Data.List

intSqrt :: Int -> Double
intSqrt = sqrt . fromIntegral

isInt :: Double -> Bool
isInt num = num == (fromIntegral . floor $ num)

isSqrt :: Int -> Bool
isSqrt num = isInt . intSqrt $ num

getInit :: Int -> (Double, (Double, Double), [(Double, Double)])
getInit num = (fromIntegral num, (1, d), [])
  where d = fromIntegral . floor . intSqrt $ num

safelast :: [(Double, Double)] -> (Double, Double)
safelast lst =
  case lst == [] of
    True -> (0,0)
    False -> last lst

process :: (Double, (Double, Double), [(Double, Double)]) -> Int
process (origin, (num, denom), soFar) =
  let
    next = fromIntegral $ floor (num / (sqrt origin - denom))
    num' = (origin - (denom^2)) / num
    denom' = num' * next - denom
  in
    case (safelast soFar) == (num', denom') of
      True -> length soFar
      False -> process (origin, (num', denom'), (num', denom') : soFar)

result :: Int
result = length . filter (not . even) . map (process . getInit) . filter (not . isSqrt) $ [1..10000]

main :: IO ()
main = do
  print result
