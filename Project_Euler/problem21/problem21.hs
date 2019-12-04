d :: Int -> Int
d x = sum . filter divisor $ [1..(x `div` 2)]
  where divisor a = x `mod` a == 0

amicables :: [Int] -> [Int]
amicables ns = map fst [(a, b) | a <- ns, b <- ns, a /= b, d a == b, d b == a]

main :: IO ()
main = do
  let result = sum . amicables $ [1..10000]
  print result
