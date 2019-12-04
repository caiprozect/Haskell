import qualified Data.Array.Accelerate as A
--import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.LLVM.PTX
import qualified Data.Vector as V

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

isPrime :: Int -> Bool
isPrime x =
  case x < 2 of
    True -> False
    False ->
      case primeVec V.! (x-2) of
        0 -> False
        otherwise -> True

main :: IO ()
main = do
  n <- getLine
  let num = read n
  print $ isPrime num
  m <- getLine
  let num' = read m
  print $ isPrime num'
