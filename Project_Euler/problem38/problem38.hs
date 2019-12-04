import Data.List

pandigits :: Int
pandigits = maximum . map check $ [1..10^4]
  where
    genPandigits l x = show $ l * x
    check x = checkWith [genPandigits 2 x, show x] 2 x
    checkWith lst l x =
      case length (concat lst) > 9 of
        True -> 0
        False ->
          case sort (concat lst) == ['1'..'9'] of
            True -> read . concat . reverse $ lst
            False -> checkWith (genPandigits (l+1) x : lst) (l+1) x 

main :: IO ()
main = do
  print pandigits
