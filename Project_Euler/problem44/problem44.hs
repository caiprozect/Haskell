import Data.List
import Control.Monad

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x)

isPentagonal :: Integer -> Bool
isPentagonal x = isInt $ (1 + sqrt (1 + 24*x')) / 6
  where x' = fromIntegral x

diffLst = [4, 7..]

sumTill :: Integer -> [Integer] -> Integer
sumTill x lst =
  case head lst > x of
    True -> 0
    False ->
      case (sum . take 2 $ lst) > x of
        True -> head lst
        False -> sumTill x ((sum . take 2 $ lst) : (drop 2 lst))

isDiffSumOf :: Integer -> [Integer] -> Integer -> (Integer, Bool)
isDiffSumOf d lst x =
  case head lst >  x of
    True -> (0, False)
    False ->
      case sumTill x lst == x of
        True -> (d, True)
        False -> isDiffSumOf (d+1) (tail lst) x

pentagonals = map (\x -> x*(3*x-1) `div` 2) [1..]

isSDPentagonal :: Integer -> [Integer] -> Integer -> Bool
isSDPentagonal d lst x =
  let
    (idx, checksum) = isDiffSumOf 1 lst x
    init = (idx+d-1)*(3*(idx+d-1)-1) `div` 2
  in
    case checksum of
      True ->
        case isPentagonal (x + 2*init) of
          True -> True
          False -> isSDPentagonal (idx+d) (genericDrop idx lst) x
      False -> False

isSDPentagonalM :: Integer -> [Integer] -> Integer -> IO ()
isSDPentagonalM d lst x = do
  let (idx, checksum) = isDiffSumOf d lst x
  let init = idx*(3*idx-1) `div` 2
  case checksum of
    True -> do
      print idx
      print $ (x, init, x+2*init)
      case isPentagonal (x + 2*init) of
        True -> putStrLn "Succeed!!!!"
        False -> isSDPentagonalM (idx+1) (genericDrop idx lst) x
    False -> return ()

pentagonNum = head . filter (isSDPentagonal 1 diffLst) $ pentagonals
  
main :: IO ()
main = do
  print pentagonNum
