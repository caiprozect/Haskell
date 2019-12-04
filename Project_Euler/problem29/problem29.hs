import Data.List

distinctPowers = length . group . sort $ ([a^b | a <- [2..100], b <- [2..100]] :: [Integer])

main :: IO ()
main = do
  print distinctPowers
