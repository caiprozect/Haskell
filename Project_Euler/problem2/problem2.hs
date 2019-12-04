{-# LANGUAGE BangPatterns #-}

maxN = 4000000 :: Int

evenFN :: Int -> (Int, Int) -> Int
evenFN acc (a, b) = let c = a+b in
  case c < maxN of
    True ->
      case c `mod` 2 == 0 of
        True -> evenFN (acc+c) (b, c)
        False -> evenFN acc (b, c)
    False -> acc

main :: IO ()
main = do
  print $ evenFN 2 (1,2)
