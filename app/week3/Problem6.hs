module Problem6 where

import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.IO

-- Given two sequences a1, a2.. an (ai is the profit per click of the i-th ad) and b1, b2, . . . , bn (bi is
-- the average number of clicks per day of the i-th slot), we need to partition them into n pairs (ai, bi))
-- such that the sum of their products is maximized.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \nums ->
  putStrLn $ show $ solve nums 

solve :: [Integer] -> Integer
solve nums =
  let strings = join $ map show nums
      digits = map (\s -> read $ s : []) strings
      getDigit num a = num * 10 + a
  in foldl' getDigit 0 $ sortBy (flip compare) digits

nextNums :: (Integral a, Read a) => Int -> IO [a]
nextNums n = sequence $ take n $ repeat nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char || (null n && char == '-')) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
