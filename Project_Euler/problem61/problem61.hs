import Data.List
import Control.Monad

commonLst :: [String]
commonLst = [[a,b,c,d] | a <- ['1'..'9'], b <- ['0'..'9'], c <- ['0'..'9'], d <- ['0'..'9']]

isInt :: Double -> Bool
isInt num = num == (fromIntegral . round $ num)

isGeom :: Int -> [String] -> [String]
isGeom 3 str =
    filter (\x -> isInt $ ((-1)+sqrt (1+8*read x)) / 2) str 
isGeom 4 str =
    filter (\x -> isInt $ sqrt (read x)) str
isGeom 5 str =
    filter (\x -> isInt $ (1+sqrt (1+24*read x)) / 6) str
isGeom 6 str =
    filter (\x -> isInt $ (1+sqrt (1+8*read x)) / 4) str
isGeom 7 str =
    filter (\x -> isInt $ (3+sqrt (9+40*read x)) / 10) str
isGeom 8 str =
    filter (\x -> isInt $ (2+sqrt (4+12*read x)) / 6) str

geomLst :: [[String]]
geomLst = zipWith (\f x -> f x) (map (isGeom) [3..8]) (repeat commonLst) 

isCycle :: [String] -> Bool
isCycle lst = nub lst == lst && sort (map (take 2) lst) == sort (map (drop 2) lst)

genCycle :: [Int] -> [[String]] -> [[String]]
genCycle [] acc = acc
genCycle [x] acc = do
  let fstSeg = take 2 . head . head $ acc
  soFar <- acc
  let lastElem = last soFar
  let candi = filter (\digit -> isPrefixOf (drop 2 lastElem) digit && isSuffixOf fstSeg digit) (geomLst !! (x-1))
  new <- candi
  genCycle [] [soFar ++ [new]] 
genCycle ints acc = do
  let fstSeg = take 2 . head . head $ acc
  soFar <- acc
  let lastElem = last soFar
  let candi = filter (isPrefixOf (drop 2 lastElem)) (geomLst !! (head ints - 1))
  new <- candi
  genCycle (tail ints) [soFar ++ [new]]
  
cfn :: [[String]]
cfn = do
  ints <- permutations [2..6]
  start <- geomLst !! 0
  genCycle ints [[start]]

main :: IO ()
main = do
  print $ sum . map read . head $ cfn

    
    
