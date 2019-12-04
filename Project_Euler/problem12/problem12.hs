import Control.Parallel.Strategies

highlyDivisibleTri :: Int
highlyDivisibleTri = head . filter check $ [1..] 
  where check x = 250 < (length $ (filter (f $ (x*(x+1) `div` 2)) [1..(x `div` 2)]))
        f y a = 0 == y `mod` a

main :: IO ()
main = do
  let x = highlyDivisibleTri
  print $ x * (x+1) `div` 2
