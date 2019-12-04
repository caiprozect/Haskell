coins = [1,2,5,10,20,50,100,200] :: [Int]

step :: [(Int, [Int])] -> [(Int, [Int])]
step lst = 
  current <- lst
  coin <- snd current
  case fst current == 0 of
    True -> [current]
    False ->
      case fst current - coin < 0 of
        True -> []
        False -> return (fst current - coin, dropWhile (/=coin) (snd current))

findBalance :: (Int, [(Int, [Int])]) -> (Int, [(Int, [Int])])
findBalance len [] = len 
findBalance len lst = findBalance (len', lst')
  where lst' = filter (\x -> fst x /= 0) $ step lst
        len' = len + (length (step lst) - length lst')

main :: IO ()
main = do
  print $ findBalance $ (0, [(200, coins)])
