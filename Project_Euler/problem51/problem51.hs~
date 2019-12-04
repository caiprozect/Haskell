import qualified Data.Array.Accelerate as A
--import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.LLVM.PTX
import qualified Data.Vector as V
import Data.List
import Control.Parallel.Strategies

sieve :: Int -> A.Acc (A.Vector Int) -> A.Acc (A.Vector Int)
sieve p as = A.map f as
  where
    p' = A.constant p
    f x = A.cond (x `mod` p' A.== 0) (g x) x
    g x = A.cond (x `div` p' A.== 1) x 0

primesUnder :: Int -> A.Acc (A.Vector Int)
primesUnder num = do
  let sieveBound = floor . sqrt . fromIntegral $ num
  let bound = A.constant (num-1)
  let arr = A.generate (A.index1 bound) $ \ix ->
        let (A.Z A.:. i) = A.unlift ix
        in i+2
  let sieves = map sieve [2..sieveBound]
  let combinedSieves = foldl1 (A.>->) sieves
  combinedSieves $ arr

primeVec = V.fromList . A.toList . run $ primesUnder 1000000

primesWDigits :: Int -> [Int]
primesWDigits n = V.toList . V.filter (/=0) . V.slice (10^(n-1)-2) (9*10^(n-1)) $ primeVec

isPrime :: Int -> Bool
isPrime x =
  case x < 2 of
    True -> False
    False ->
      case primeVec V.! (x-2) of
        0 -> False
        otherwise -> True

maskDigit :: Int -> Int -> Int
maskDigit d p = p - dth*10^(d-1)
  where
    dth = p `div` 10^(d-1) `mod` 10

maskDigits :: [Int] -> Int -> Int
maskDigits ds p =
  let
    dth d = p `div` 10^(d-1) `mod` 10
    maskeds = map dth ds
  in
    case length (group maskeds) == 1 of
      True -> foldl1 (.) (map maskDigit ds) $ p
      False -> p

orderByMasked :: [Int] -> Int -> Int -> Ordering
orderByMasked ds p1 p2 = compare (maskDigits ds p1) (maskDigits ds p2)

eqWhenMasked :: [Int] -> Int -> Int -> Bool
eqWhenMasked ds p1 p2 = (maskDigits ds p1) == (maskDigits ds p2)

safeMinimum :: [Int] -> Maybe Int
safeMinimum [] = Nothing
safeMinimum lst = Just $ minimum lst

digitReplacementsP :: Int -> [Int] -> Maybe Int
digitReplacementsP n ds = safeMinimum . map head . map sort . filter check . groupBy (eqWhenMasked ds) . sortBy (orderByMasked ds) $ primesWDigits n
  where
    check lst = length lst == 8

main :: IO ()
main = do
  print $ minimum . filter (/=Nothing) $ ((map (digitReplacementsP 6) $ init . tail . subsequences $ [1..6]) `using` (parBuffer 100 rdeepseq))
