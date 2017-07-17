module Problem4 where

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
  allHours n >>= \h ->
  let solved = solve h
      size = length solved
      print' = intercalate " " . map show
  in  putStrLn $ show size >>= \_ ->
      putStrLn $ print' solved

solve :: (Ord a) => [(a, a)] -> [a]
solve =
  let f [] (start, end) = end : []
      f acc@(h:_) (start, end)
        | start <= h = acc
        | otherwise = end : acc
  in (foldl' f []) . (sortBy hourOrder)

allHours n = sequence $ take n $ repeat nextHours

hourOrder (f1, t1) (f2, t2)
  | t1 > t2 = GT
  | t1 < t2 = LT
  | t1 == t2 = compare f1 f2

nextHours = fmap (tuplify . sort) $ nextNums 2

tuplify :: [a] -> (a, a)
tuplify [a, b] = (a, b)

nextNums :: (Integral a, Read a) => Int -> IO [a]
nextNums n = sequence $ take n $ repeat nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char || (null n && char == '-')) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
