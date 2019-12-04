import qualified Data.Vector as V
import Data.Char

fractional = V.take (10^6) . V.fromList $ concat . map show $ [1..]

prod = product $ map (digitToInt . ((V.!) fractional)) $ map (\x -> 10^x - 1) [0..6]

main :: IO ()
main = do
  print prod
