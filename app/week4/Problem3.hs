module Problem3 where

import Data.Char
import Data.List
import Control.Applicative
import System.IO

-- The goal in this problem is to find the minimum number of coins needed to change the input value
-- (an integer) into coins with denominations 1, 5, and 10.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \nums ->
  putStrLn $ toString $ quicksort3 nums

quicksort3 :: (Ord a) => [a] -> [a]
quicksort3 [] = []
quicksort3 list@(h:[]) = list
quicksort3 list@(h:t) =
  let (l, e, g) = foldl' (f h) ([], [], []) list
  in (quicksort3 l) ++ e ++ (quicksort3 g)

f c (l, e, g) a
  | a > c = (l, e, a:g)
  | a == c = (l, a:e, g)
  | a < c = (a:l, e, g)

toString = intercalate " " . map show

nextNums n = sequence $ replicate n nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
