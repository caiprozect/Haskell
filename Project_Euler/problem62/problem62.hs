import qualified Data.Set as S
import Data.List
import Math.NumberTheory.Powers.Cubes

cubes :: [Int]
cubes = [n^3 | n <- [1..99999]]

numToPaddedStr :: String -> String
numToPaddedStr num =
  case (length . show . last $ cubes) > (length num) of
    True -> numToPaddedStr $ '0' : show num
    False -> show num

permOrder :: Int -> Int -> Ordering
permOrder n1 n2 = compare sn1 sn2
  where
    sn1 = sort . numToPaddedStr . show $ n1
    sn2 = sort . numToPaddedStr . show $ n2

permEq :: Int -> Int -> Bool
permEq n1 n2 = sn1 == sn2
  where
    sn1 = sort . numToPaddedStr . show $ n1
    sn2 = sort . numToPaddedStr . show $ n2

minimumCP :: Int
minimumCP = head . head . filter check . sort . map sort . groupBy permEq . sortBy permOrder $ cubes
  where
    check ns = length ns == 5

main :: IO ()
main = do
  print minimumCP

