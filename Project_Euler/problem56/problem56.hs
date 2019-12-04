import Data.Char

powerfulDigitSum :: Int
powerfulDigitSum = maximum . map (sum . map digitToInt . show) $ lst
  where lst = [a^b | a <- [1..100], b <- [1..100]] :: [Integer]

main :: IO ()
main = do
  print powerfulDigitSum
