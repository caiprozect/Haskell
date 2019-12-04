import Data.List
import Control.Monad

permutedMultiple :: Int
permutedMultiple = head . filter check $ [n | n <- [1..], (head . show $ n) == '1'] 
  where check x = all ((sort . show $ x) ==) . map (sort . show) $ zipWith (*) [2..6] (repeat x)
  
main :: IO ()
main = do
  print permutedMultiple
