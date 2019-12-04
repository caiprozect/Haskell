fDigitToString :: Int -> String
fDigitToString num =
  case num of
    0 -> ""
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"
sDigitToString :: Int -> String
sDigitToString num =
  case num < 20 of
    True -> fDigitToString num
    False -> 
      case num `div` 10 of
        2 -> "twenty" ++ fDigitToString (num `mod` 10)
        3 -> "thirty" ++ fDigitToString (num `mod` 10)
        4 -> "forty" ++ fDigitToString (num `mod` 10)
        5 -> "fifty" ++ fDigitToString (num `mod` 10)
        6 -> "sixty" ++ fDigitToString (num `mod` 10)
        7 -> "seventy" ++ fDigitToString (num `mod` 10)
        8 -> "eighty" ++ fDigitToString (num `mod` 10)
        9 -> "ninety" ++ fDigitToString (num `mod` 10)
tDigitToString :: Int -> String
tDigitToString num =
  case num < 100 of
    True -> sDigitToString num
    False ->
      case num `mod` 100 == 0 of
        True -> fDigitToString (num `div` 100) ++ "hundred"
        False -> fDigitToString (num `div` 100) ++ "hundredand" ++ sDigitToString (num `mod` 100)

digitToString :: Int -> String
digitToString num =
  case num < 1000 of
    True -> tDigitToString num
    False -> "onethousand"

main :: IO ()
main = do
  let lst = [1..1000]
  let result = length . concat . map digitToString $ lst
  print result
        
