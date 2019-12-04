smallestMultiple :: Int
smallestMultiple = head . filter f $ [1..]
  where f x = all (==0) . map (mod x) $ [1..20]

main :: IO ()
main = do
  print smallestMultiple
