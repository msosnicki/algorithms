module Problem2 where

import Data.Char
import Data.List
import Data.Maybe
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
  nextNums n >>= \nums ->
  putStrLn $ show $ majority nums


majority :: (Eq a) => S.Seq a -> Int
majority s =
  let majorityElement = majority' s 0 (S.length s)
  in if(isJust majorityElement) then 1 else 0

majority' :: (Eq a) => S.Seq a -> Int -> Int -> Maybe a
majority' s from to
  | span == 0 = Nothing
  | span == 1 = Just $ S.index s from
  | otherwise = 
      let ml = majority' s from mid
          mr = majority' s mid to
          cl = fmap (count s from to) ml
          cr = fmap (count s from to) mr
          gl = gtOpt cl $ span `div` 2
          gr = gtOpt cr $ span `div` 2
      in (gl >> ml) <|> (gr >> mr)
  where
    span = to - from
    mid = (from +) $ span `div` 2

gtOpt o n = o >>= \v -> if(v>n) then Just v else Nothing

subseq from to s = S.drop from $ S.take to s

filterM pred m = m >>= \v -> if(pred v) then pure m else Nothing

count :: (Eq a) => S.Seq a -> Int -> Int -> a -> Int
count seq from to element =
  let sub = subseq from to seq
  in F.foldl' (\acc -> \e -> if(e == element) then acc + 1 else acc) 0 sub

nextNums n = T.sequence $ S.replicate n nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
