import Data.Time.Calendar

firstOfMonths = [(year, month, 1) | year <- [1901..2000], month <- [1..12]]

main :: IO ()
main = do
  let start = fromGregorian 1900 1 1
  let check (y, m, d) = (diffDays (fromGregorian y m d) start) `mod` 7 == 6 
  let result = length $ filter check firstOfMonths
  print result
