triangleSolLen :: Int -> Int
triangleSolLen n = length lst
  where lst = [(a,b,c) | a <- [1..(n `div` 2)], b <- [a..(n-2*a)], let c = n - a - b, a*a + b*b == c*c]

main :: IO ()
main = do
  print $ snd . maximum $ zip (map triangleSolLen $ [1..1000]) [1..1000]
