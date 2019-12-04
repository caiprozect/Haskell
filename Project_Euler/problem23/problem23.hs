import Data.List
import qualified Data.Set as S

abundants = [n | n <- [1..28123], n < d n]
  where d x = sum . filter (divisor x) $ [1..(x `div` 2)]
        divisor x a = x `mod` a == 0

expressed = S.fromList [a + b | a <- abundants, b <- abundants, a <= b, a+b <= 28123] :: S.Set Int

main :: IO ()
main = do
  let result = (28123*28124) `div` 2 - (S.foldl' (+) 0 expressed)                    
  print result
