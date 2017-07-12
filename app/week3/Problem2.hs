module Problem2 where

import Data.Char
import Control.Applicative
import System.IO

-- The goal of this code problem is to implement an algorithm for the fractional knapsack problem.

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  -- nextNum >>= \i ->
  -- nextNum >>= \c ->
  putStrLn $ "tete"

-- nextNum = nextNum' ""

-- nextNum' n = getChar >>= \char ->
--   if(isDigit char) then nextNum' $ char:n
--   else if(null n) then nextNum' n
--   else pure $ read $ reverse n
