module Problem3 where

import Data.Char
import Data.List
import Control.Applicative
import System.IO

-- Given two sequences a1, a2.. an (ai is the profit per click of the i-th ad) and b1, b2, . . . , bn (bi is
-- the average number of clicks per day of the i-th slot), we need to partition them into n pairs (ai, bi))
-- such that the sum of their products is maximized.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \pc ->
  nextNums n >>= \ac ->
  let
    sortedPc = sortBy (flip compare) pc
    sortedAc = sortBy (flip compare) ac
    result   = sum $ zipWith (*) sortedPc sortedAc
  in putStrLn $ show $ result

nextNums n = sequence $ take n $ repeat nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char || (null n && char == '-')) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
