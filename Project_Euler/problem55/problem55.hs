import Data.List

isPalindrome :: Integer -> Bool
isPalindrome x = x == (read . reverse . show $ x :: Integer)

lynchrelStep :: Integer -> Int -> Integer
lynchrelStep x 0 = x + (read . reverse . show $ x :: Integer)
lynchrelStep x _ = 
  case isPalindrome x of
    True -> x
    False -> x + (read . reverse . show $ x :: Integer)

lynchrelCheck :: Integer -> Bool
lynchrelCheck x = not . isPalindrome $ foldl' lynchrelStep x [0..49]

main :: IO ()
main = do
  print $ length . filter lynchrelCheck $ [1..10000]
