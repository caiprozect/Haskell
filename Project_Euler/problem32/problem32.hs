import qualified Data.Set as S
import Data.List

diff :: Int -> [Int]
diff k = filter check [10^(k-1)..(10^k-1)]
  where check x = k == (length . sort . show $ x)

pattern144 :: S.Set Int
pattern144 = S.fromList ps
  where ps = [a*b | a <- [1..9], b <- (diff 4), check a b]
        check a b = (sort $ show a ++ show b ++ show (a*b)) == ['1'..'9']

pattern234 :: S.Set Int
pattern234 = S.fromList ps
  where ps = [a*b | a <- (diff 2), b <- (diff 3), check a b]
        check a b = (sort $ show a ++ show b ++ show (a*b)) == ['1'..'9']

main :: IO ()
main = do
  let s = pattern144 `S.union` pattern234
  let result = sum . S.toList $ s
  print result
