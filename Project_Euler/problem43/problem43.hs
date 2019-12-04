import Data.List

pandigits = permutations ['0'..'9']

isSubDivisible :: String -> Bool
isSubDivisible str =
  let
    subs = zipWith (\k w -> take 3 . drop k $ w) [1..7] (repeat str)
  in
  case head str == '0' of
    True -> False
    False -> all (==0) $ zipWith mod (map read $ subs) [2, 3, 5, 7, 11, 13, 17]

main :: IO ()
main = do
  print $ sum . map read . filter isSubDivisible $ pandigits
