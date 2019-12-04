import Data.List

primes = sieve [2..]
  where
    sieve lst = (head lst) : sieve (filter (\x -> x `mod` (head lst) /= 0) lst)

factorLen :: Int -> [Int] -> Int -> Int -> Int
factorLen len (p:ps) prev 1 = len
factorLen len (p:ps) prev x =
  case x `mod` p == 0 of
    True ->
      case prev == p of
        True -> factorLen len (p:ps) prev (x `div` p)
        False -> factorLen (len+1) (p:ps) p (x `div` p)
    False -> factorLen len ps prev x

distinctPF :: Int -> Int
distinctPF x = factorLen 0 primes 1 x

consecDPF :: Int -> Int
consecDPF num = findConsec . filter (\x -> distinctPF x == num) $ [210..]
  where
    findConsec (x:xs) =
      case [x..(x+num-1)] == take num (x:xs) of
        True -> x
        False -> findConsec xs

main :: IO ()
main = do
  print $ consecDPF 4
