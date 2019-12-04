import Data.List
import Data.List.Split
import System.IO

data Hand = HI | OP | TP | TK | St | Fs | FH | FK | SF | RF deriving (Show, Ord, Eq)

charToValue :: String -> Int
charToValue "2" = 2
charToValue "3" = 3
charToValue "4" = 4
charToValue "5" = 5
charToValue "6" = 6
charToValue "7" = 7
charToValue "8" = 8
charToValue "9" = 9
charToValue "T" = 10
charToValue "J" = 11
charToValue "Q" = 12
charToValue "K" = 13
charToValue "A" = 14

splitVS :: String -> (Int, String)
splitVS str = (charToValue . take 1 $ str, drop 1 str)

sameSuit :: [(Int, String)] -> Bool
sameSuit lst = 1 == (length . group . map snd $ lst)

royal :: [(Int, String)] -> Bool
royal lst = [10..14] == (sort . map fst $ lst)

straight :: [(Int, String)] -> Bool
straight lst = [k..k+4] == vals
  where
    vals = sort . map fst $ lst
    k = head vals

compareLen :: [a] -> [a] -> Ordering
compareLen lst1 lst2 = compare (length lst1) (length lst2)

valSplitTwo :: [(Int, String)] -> (Bool, Int)
valSplitTwo lst = (2 == length valLst, head . last . sortBy compareLen $ valLst)
  where valLst = group . sort . map fst $ lst

fourKinds :: [(Int, String)] -> Bool
fourKinds lst = (4 == (length . last . sortBy compareLen $ valLst))
  where valLst = group . sort . map fst $ lst

threeKinds :: [(Int, String)] -> Bool
threeKinds lst = (3 == (length . last . sortBy compareLen $ valLst))
  where valLst = group . sort . map fst $ lst

lenCompare :: [a] -> [a] -> Bool
lenCompare lst1 lst2 = length lst1 == length lst2

twoPairs :: [(Int, String)] -> (Bool, Int)
twoPairs lst = (2 == (length . last . groupBy lenCompare . sortBy compareLen $ valLst), head . last . sort . last . groupBy lenCompare . sortBy compareLen $ valLst) 
  where valLst = group . sort . map fst $ lst

onePair :: [(Int, String)] -> Bool
onePair lst = 5 /= (length . group . sort . map fst $ lst)

evalHand :: String -> (Hand, Int, Int)
evalHand str =
  let
    svs = map splitVS . splitOn " " $ str
    highest = maximum . map fst $ svs
  in
  case sameSuit svs of
    True ->
      case royal svs of
        True -> (RF, highest, highest)
        False ->
          case straight svs of
            True -> (SF, highest, highest)
            False -> (Fs, highest, highest)
    False ->
      case straight svs of
        True -> (St, highest, highest)
        False ->
          case fst (valSplitTwo svs) of
            True ->
              case fourKinds svs of
                True -> (FK, snd (valSplitTwo svs), highest)
                False -> (FH, snd (valSplitTwo svs), highest)
            False ->
              case threeKinds svs of
                True -> (TK, snd (valSplitTwo svs), highest)
                False ->
                  case fst (twoPairs svs) of
                    True -> (TP, snd (twoPairs svs), highest)
                    False ->
                      case onePair svs of
                        True -> (OP, snd (twoPairs svs), highest)
                        False -> (HI, highest, highest)

winnerIs :: String -> Int
winnerIs game =
  let
    p1 = evalHand . take 14 $ game
    p2 = evalHand . take 14 . drop 15 $ game
  in
    case p1 > p2 of
      True -> 1
      False -> 2

main :: IO ()
main = do
  withFile "poker.txt" ReadMode $ \handle -> do
    text <- hGetContents handle
    let lns = lines text
    let num = length . filter (==1) . map winnerIs $ lns
    print num
              
          
      
      
