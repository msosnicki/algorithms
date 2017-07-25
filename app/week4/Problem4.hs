module Problem4 where

import Data.Char
import Data.List
import Control.Applicative
import System.IO
-- import qualified Data.Sequence as S
-- import qualified Data.Traversable as T

-- The goal in this problem is to find the minimum number of coins needed to change the input value
-- (an integer) into coins with denominations 1, 5, and 10.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \nums ->
  putStrLn $ show $ fst $ solve nums

solve l = solve' l

solve' [] = (0, [])
solve' l@(h:[]) = (0, l)
solve' l =
  let (leftS, rightS) = (solve' left, solve' right)
      in merge leftS rightS
  where size = length l
        (left, right) = splitAt (size `div` 2) l

merge l r =
  let (count, merged) = merge' (fst l + fst r) [] (snd l) (snd r)
  in (count, reverse merged)
merge' c acc [] r = (c, foldl' (flip (:)) acc r)
merge' c acc l [] = (c, foldl' (flip (:)) acc l)
merge' c acc l@(hl:tl) r@(hr:tr)
  | hl <= hr = merge' (c) (hl : acc) tl r
  | otherwise = merge' ((c+) $ length l) (hr : acc) l tr
-- solve :: (Ord a) => S.Seq a -> Int
-- solve l = fst $ solve' l

-- solve' (S.viewl -> S.EmptyL) = (0, S.empty)
-- solve' s
--   | size == 1 = (0, s)
--   | otherwise =
--     let (left, right) = S.splitAt (size `div` 2) s
--         solvedl = solve' left
--         solvedr = solve' right
--         merged = merge solvedl solvedr
--     in merged
--   where size = S.length s

-- merge :: (Ord a) => (Int, S.Seq a) -> (Int, S.Seq a) -> (Int, S.Seq a)
-- merge l r = merge' (fst l + fst r, S.empty) l r
-- merge' (c, acc) (cl, S.viewl -> S.EmptyL) (cr, S.viewl -> S.EmptyL) = (c, acc)
-- merge' (c, acc) (cl, S.viewl -> S.EmptyL) r = ((c+) $ S.length r, acc)
-- merge' (c, acc) l (cr, S.viewl -> S.EmptyL) = (c )

nextNums n = sequence $ replicate n nextNum

nextNum :: (Integral a, Read a) => IO a
nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
