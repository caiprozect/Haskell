import Math.Combinatorics.Exact.Binomial

combSel :: Int
combSel = length . filter (>1000000) $ [choose n r | n <- [1..100], r <- [0..n]]

main :: IO ()
main = do
  print combSel
