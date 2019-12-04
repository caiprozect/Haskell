import Data.Char

main :: IO ()
main = do
  let x = product [1..100]
  let result = sum . map digitToInt . show $ x
  print result
