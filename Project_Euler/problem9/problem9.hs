import Data.Char (digitToInt)

specialPT :: Int
specialPT = head [a*b*c | b <- [999,998..1], a <- [(b-1),(b-2)..1], let c=1000-a-b, a*a+b*b==c*c]

main :: IO ()
main = do
  print specialPT
