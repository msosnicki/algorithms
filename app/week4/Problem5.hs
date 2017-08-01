{-# LANGUAGE ViewPatterns #-}
module Problem5 where

import Data.Char
import Data.List
import Control.Applicative
import System.IO

-- The goal in this problem is to find the minimum number of coins needed to change the input value
-- (an integer) into coins with denominations 1, 5, and 10.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \r ->
  nextNum >>= \p ->
  nextRanges r >>= \ranges ->
  nextNums p >>= \points ->
  putStrLn $ printable $ solve ranges points

printable l = intercalate " " $ map show l

solve r p =
  let points = map (\p -> (p, Point)) p
      ranges = r >>= (\r -> [(fst r, Up), (snd r, Down)])
      sorted = sortBy compareFst (points ++ ranges)
      solved = foldl' counts ([], 0) sorted
  in reverse $ fst solved

counts (acc, c) (p, Point) = ((p, c) : acc, c)
counts (acc, c) (p, Up) = (acc, c + 1)
counts (acc, c) (p, Down) = (acc, c - 1)

compareFst (p1, t1) (p2, t2)
  | p1 == p2 = compare t1 t2
  | otherwise = compare p1 p2

next a n = sequence $ replicate n a

nextRange =
  nextNum >>= \n1 ->
  fmap (\n2 -> if(n2>=n1) then (n1, n2) else (n2, n1)) nextNum

nextRanges = next nextRange
nextNums = next nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char || (null n && char == '-')) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n

data ElementType = Up | Point | Down deriving (Show, Eq, Ord)
