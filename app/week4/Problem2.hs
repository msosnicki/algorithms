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
  putStrLn "tete"


majority :: (Eq a) => S.Seq a -> Maybe Int
majority s = majority' s 0 (S.length s)

majority' :: (Eq a) => S.Seq a -> Int -> Int -> Maybe Int
majority' s from to
  | from >= to = Nothing
  | from + 1 == to = Just from
  | otherwise = 
      let span = to - from
          mid = (from +) $ span `div` 2 
          majL = majority' s from mid
          majR = majority' s mid to
          sub = subseq from to s
          pred = \elem -> elem > (span `div` 2)
          cl = filterM pred $ fmap (count sub) majL
          cr = filterM pred $ fmap (count sub) majR
      in 

subseq from to s = S.drop from $ S.take to s

filterM pred m = m >>= \v -> if(pred v) then pure m else Nothing

count :: (Eq a) => S.Seq a -> Int -> Int
count seq i =
  let element = S.index seq i
  in F.foldl' (\acc -> \e -> if(e == element) then acc + 1 else acc) 0 seq

nextNums n = T.sequence $ replicate n nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
