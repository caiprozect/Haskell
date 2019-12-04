import qualified Data.Vector as V
import Data.Char

factoVector :: V.Vector Int
factoVector = V.generate 10 facto
  where facto 0 = 1
        facto n = product [1..n]

lst = [n | n <- [3..999999], n == (sum . map (\c -> factoVector V.! (digitToInt c)) . show $ n)]

main :: IO ()
main = do
  let result = sum lst
  print lst
  print result
