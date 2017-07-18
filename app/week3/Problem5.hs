module Problem5 where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import System.IO

-- Given two sequences a1, a2.. an (ai is the profit per click of the i-th ad) and b1, b2, . . . , bn (bi is
-- the average number of clicks per day of the i-th slot), we need to partition them into n pairs (ai, bi))
-- such that the sum of their products is maximized.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  let solved = consUpTo 1 n
      size = length solved
      print' = intercalate " " . map show
  in (putStrLn $ show size) >>= \_ ->
     putStrLn $ print' solved

consUpTo lb up
  | (2*lb) + 1 > up = up : []
  | otherwise = lb : consUpTo (lb + 1) (up - lb)

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char || (null n && char == '-')) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
