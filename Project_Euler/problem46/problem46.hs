isInt :: Double -> Bool
isInt x = x == fromIntegral (round x)

isSquare :: Int -> Bool
isSquare x = isInt . sqrt $ x'
  where x' = fromIntegral x

isPrime :: Int -> Bool
isPrime x = all (/=0) . map (x `mod`) $ [2..(floor . sqrt . fromIntegral $ x)]

goldbachCounter = head . filter check $ [9, 11..]
  where check x =
          case isPrime x of
            True -> False
            False ->
              all (not . isSquare) $ zipWith (\x y -> (x - y) `div` 2) (repeat x) (filter isPrime [3..x])

main :: IO ()
main = do
  print goldbachCounter
