digitFifthPower :: [Int]
digitFifthPower = do
  a <- [0..9]
  b <- [0..9]
  c <- [0..9]
  d <- [0..9]
  e <- [0..9]
  f <- [0..9]
  let digitPS = sum . map (\x -> x^5) $ [a,b,c,d,e,f]
  let num = a*10^5 + b*10^4 + c*10^3 + d*10^2 + e*10 + f
  case digitPS == num of
    True -> [num]
    False -> []

main :: IO ()
main = do
  print digitFifthPower
  print $ sum digitFifthPower - 1
