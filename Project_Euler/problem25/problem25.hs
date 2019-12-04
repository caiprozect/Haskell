upper = 10^999 :: Integer

fib :: (Integer, Integer) -> (Integer, Integer)
fib (f1, f2) =
  let
    f3 = f1+f2
  in
    f3 `seq` (,) f2 f3

thousandDigits :: (Int, (Integer, Integer)) -> Int 
thousandDigits (x, (a, b)) =
  case a >= upper of
    True -> x
    False -> thousandDigits $ (x+1, fib (a, b))

main :: IO ()
main = do
  let result = thousandDigits (1, (1, 1))
  print result

