import Data.List

isPrime :: Integer -> Bool
isPrime n =
 case n < 2 of
   True -> False
   False -> all (/=0) . map (n `mod`) $ [2..(floor . sqrt . fromIntegral $ n)]

quadLength :: Integer -> Integer -> Integer
quadLength a b = genericLength . takeWhile isPrime . map (\x -> x*x + a*x + b) $ [0..]

prodMaxQuadLeng = prod . maximum $ [(len, (a, b)) | a <- [-999..999], b <- [0..1000], let len = quadLength a b]
  where prod tu = (fst . snd $ tu) * (snd . snd $ tu)

main :: IO ()
main = do
  print $ quadLength (-79) 1601
  print prodMaxQuadLeng
