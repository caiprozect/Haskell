num = 10000000 :: Int

longestCollatz :: [(Int, Int)] -> [(Int, Int)]
longestCollatz [t] = [t]
longestCollatz ts = longestCollatz . map collatz $ filter check ts
  where
    collatz (init, next) =
      case even next of
        True -> (init, next `div` 2)
        False -> (init, 3*next + 1)
    check (init, next) = 1 /= next

main :: IO ()
main = do
  let result = fst . head . longestCollatz $ zip [1..num] [1..num]
  print result
