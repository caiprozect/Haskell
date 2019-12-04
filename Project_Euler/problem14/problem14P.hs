import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX
--import Data.Array.Accelerate.Interpreter

num = 10000000 :: A.Exp Int

genCollatz :: A.Acc (A.Vector (Int, Int)) -> A.Acc (A.Vector (Int, Int))
genCollatz arr =
  A.map collatz . A.afst . A.filter check $ arr
    where
      check x = (A.snd x) A./= (1 :: A.Exp Int)
      collatz x =
        A.cond
        (A.even . A.snd $ x)
        (A.lift (A.fst x, A.snd x `div` 2) :: A.Exp (Int, Int))
        (A.lift (A.fst x, 3*(A.snd x) + 1) :: A.Exp (Int, Int)) 

longestCollatz :: A.Acc (A.Vector (Int, Int)) -> A.Acc (A.Vector (Int, Int))
longestCollatz arr = A.awhile check genCollatz arr
   where check arr = A.unit $ A.length arr A.> 1
  
main :: IO ()
main = do
  let result = run . longestCollatz $ A.zip (A.enumFromN (A.index1 num) 1) (A.enumFromN (A.index1 num) 1)
  print result
