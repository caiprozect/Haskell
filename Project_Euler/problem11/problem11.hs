import System.IO
import Data.Array.Accelerate as A
import Prelude as P
--import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.Interpreter

inputFile = "input.txt"

largestPinG :: [Int] -> Acc (Scalar Int)
largestPinG lst = A.maximum . genPs $ arr
  where
    arr = use (A.fromList (Z:.24:.24) lst :: Array DIM2 Int)
    genPs arr = generate sh gen
    (Z:.n:.m) = unlift $ shape arr
    sh = lift (Z:.((n-8) :: Exp Int):.((m-4) :: Exp Int))
    gen :: Exp DIM2 -> Exp Int
    gen ix = let
      (Z:.i':.j) = unlift ix
      i = i'+ 4 :: Exp Int
      in
      A.max
        (A.max (A.max (arr ! (index2 i j) * arr ! (index2 (i+1) j) * arr ! (index2 (i+2) j) * arr ! (index2 (i+3) j))
              (arr ! (index2 i j) * arr ! (index2 i (j+1)) * arr ! (index2 i (j+2)) * arr ! (index2 i (j+3))))
              (arr ! (index2 i j) * arr ! (index2 (i+1) (j+1)) * arr ! (index2 (i+2) (j+2)) * arr ! (index2 (i+3) (j+3))))
        (arr ! (index2 i j) * arr ! (index2 (i-1) (j+1)) * arr ! (index2 (i-2) (j+2)) * arr ! (index2 (i-3) (j+3)))
        
main :: IO ()
main = do
  withFile inputFile ReadMode $ \handle -> do
    text <- hGetContents handle
    let lns = lines text
    let colPaded = P.map words . P.map (P.++" 00 00 00 00") $ lns
    let padded = (P.take 4 . repeat . P.take 24 $ repeat "00") P.++ colPaded P.++ (P.take 4 . repeat . P.take 24 $ repeat "00")
    let lst = P.map read . concat $ padded :: [Int]
    let result = run $ largestPinG lst
    print result
