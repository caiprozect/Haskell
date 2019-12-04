lenRecurCycle :: Int -> [Int] -> (Int, Int)
lenRecurCycle n rs =
  let r = (head rs)*10 `mod` n in
  case elem r rs || r == 0 of
    True -> (length rs, n)
    False -> lenRecurCycle n (r:rs)  

lenRC n = lenRecurCycle n [1]

main :: IO ()
main = do
  let result = snd . maximum . map lenRC $ [1..999]
  print result
