triangle n = n * (n+1) `div` 2

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x)

isPentagonal :: Int -> Bool
isPentagonal x = isInt $ (1 + sqrt (1 + 24*x')) / 6
  where x' = fromIntegral x

isHexagonal :: Int -> Bool
isHexagonal x = isInt $ (1 + sqrt (1 + 8*x')) / 4
  where x' = fromIntegral x

tph = head . filter (\x -> isPentagonal x && isHexagonal x) . map triangle $ [286..]

main :: IO ()
main = do
  print tph
