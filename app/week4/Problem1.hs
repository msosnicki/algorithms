{-# LANGUAGE ViewPatterns #-}
module Problem1 where

import Data.Char
import Data.List
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Control.Applicative
import System.IO

-- The goal in this problem is to find the minimum number of coins needed to change the input value
-- (an integer) into coins with denominations 1, 5, and 10.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \a ->
  nextNum >>= \k ->
  nextNums k >>= \b ->
  let result = fmap (show . binarySearch a) b
  in putStrLn $ intercalate " " $ F.toList result

binarySearch s a = binarySearch' s a 0 (S.length s)

binarySearch' :: (Ord a) => S.Seq a -> a -> Int -> Int -> Int
binarySearch' (S.viewl -> S.EmptyL) _ _ _= -1
binarySearch' s a low high
  | low == high = -1
  | a == mid = i
  | a < mid = binarySearch' s a low i
  | a > mid = binarySearch' s a (i + 1) high
  where i = (low +) $ div (high - low) 2
        mid = S.index s i

nextNums :: (Integral a, Read a) => Int -> IO (S.Seq a)
nextNums n = T.sequence $ S.replicate n nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
