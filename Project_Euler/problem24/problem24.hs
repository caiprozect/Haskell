import Data.List

digits = ['0'..'9']

main :: IO ()
main = do
  let result = head . drop 999999 . sort . permutations $ digits
  print result
