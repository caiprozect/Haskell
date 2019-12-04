step :: Integer -> Integer -> Integer -> Integer
step i t 500 = t
step i t s = step (last corner) (t + sum corner)  (s+1)
  where corner = map (\x -> x*2*(s+1) + i) [1..4]
    
main :: IO ()
main = do
  let result = step 1 1 0
  print result
