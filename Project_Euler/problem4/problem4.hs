largestPalin :: Int
largestPalin = maximum . filter isPalin $ lst
  where lst = [a * b | a <- [999, 998..100], b <- [999, 998..100]]
        isPalin x = show x == (reverse . show $ x)

main :: IO ()
main = do
  print largestPalin
