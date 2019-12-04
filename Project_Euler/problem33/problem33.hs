import Data.Ratio

lst = [a % b | a <- [1..9], b <- [1..9], k <- [1..9], a < b, a % b == (a*10 + k) % (k*10 + b)]

main :: IO ()
main = do
  let result = product lst
  print result

