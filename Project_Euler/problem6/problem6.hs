sumSD :: Int
sumSD = (square . sum $ [1..100]) - (sum . map square $ [1..100])
  where square x = x*x

main :: IO ()
main = do
  print sumSD
