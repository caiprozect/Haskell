import Data.List

selfPowers :: Integer
selfPowers = foldl1' f $ zipWith (^) [1..1000] [1..1000]
  where
    f x y = (x + y) `mod` (10^10)

main :: IO ()
main = do
  print selfPowers
