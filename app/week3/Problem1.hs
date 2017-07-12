module Problem1 where

import Data.Char
import Data.List
import Control.Applicative
import System.IO

-- The goal in this problem is to find the minimum number of coins needed to change the input value
-- (an integer) into coins with denominations 1, 5, and 10.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \money ->
  putStrLn $ show $ length $ coins money

coins = unfoldr f

f s
  | s >= 10 = Just (10, s - 10)
  | s >= 5 = Just (5, s - 5)
  | s > 0 = Just(1, s - 1)
  | otherwise = Nothing

nextNum = nextNum' ""

nextNum' n = getChar >>= \char ->
  if(isDigit char) then nextNum' $ char:n
  else if(null n) then nextNum' n
  else pure $ read $ reverse n
