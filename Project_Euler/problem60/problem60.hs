import qualified Data.Array.Accelerate as A
--import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.LLVM.PTX
import qualified Data.Vector as V
import Control.Parallel.Strategies
import Data.Numbers.Primes

{-|
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
  let sieves =
        case sieveBound <= 1000 of
          True -> map sieve [2..sieveBound]
          False -> map sieve $ filter (/= 0) . A.toList . run $ primesUnder sieveBound 
  let combinedSieves = foldl1 (A.>->) sieves
  combinedSieves $ arr

numVec = V.fromList . A.toList . run $ primesUnder 100000000

primeVec = V.filter (/= 0) numVec

primeLen = V.length primeVec

--isPrime :: Int -> Bool
--isPrime x = all (/= 0) . map (x `mod`) $ [2..floor . sqrt . fromIntegral $ x]

isPrime :: Int -> Bool
isPrime x =
  case x < 2 of
    True -> False
    False ->
      case numVec V.! (x-2) of
        0 -> False
        otherwise -> True
-}

primeVec = V.fromList . take 10000 $ primes

primeLen = V.length primeVec

concatP :: Int -> Int -> Bool
concatP a b = all isPrime [read (sa ++ sb), read (sb ++ sa)]
  where
    [pa, pb] = toPrime [a, b]
    [sa, sb] = map show [pa, pb]

toPrime :: [Int] -> [Int]
toPrime lst = map (primeVec V.!) lst

primePS = do
  a <- [5..primeLen-1]
  e <- [1..a-1]
  case concatP a e of
    False -> []
    True -> do
      b <- [e+1..a-1]
      case and $ zipWith concatP [a,e] (repeat b) of
        False -> []
        True -> do
          c <- [b+1..a-1]
          case and $ zipWith concatP [a,e,b] (repeat c) of
            False -> []
            True -> do
              d <- [c+1..a-1]
              case and $ zipWith concatP [a,e,b,c] (repeat d) of
                False -> []
                True -> return [a,e,b,c,d]         

main :: IO ()
main = do
  print $ V.take 20 primeVec
  print $ toPrime . head $ primePS
  
